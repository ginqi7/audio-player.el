;;; audio-player-mpv.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'audio-player)
(require 'audio-player-backend-api)

(defcustom audio-player-debug-p nil
  "When non-nil, log IPC commands sent to mpv process."
  :group 'audio-player-mpv)

(defcustom audio-player-mpv-extra-args nil
  "Extra arguments passed to mpv before the media URL."
  :type '(repeat string)
  :group 'audio-player-mpv)

(defcustom audio-player-mpv-network-cache-args
  '("--cache=yes"
    "--cache-pause=no"
    "--demuxer-readahead-secs=60"
    "--demuxer-max-bytes=256MiB")
  "Default mpv cache arguments used for long network tracks.
These arguments are placed before `audio-player-mpv-extra-args', so user-supplied
extra arguments can override them."
  :type '(repeat string)
  :group 'audio-player-mpv)

(defcustom audio-player-mpv-program (executable-find "mpv")
  "Program name or path used to run mpv."
  :type 'string
  :group 'audio-player-mpv)

(defclass audio-player-mpv (audio-player-backend)
  ((process :initarg :process :initform nil)
   (socket :initarg :socket :initform nil)
   (ipc-process :initarg :ipc-process :initform nil))
  "mpv-based audio player backend using Unix socket IPC.")

(defvar audio-player-mpv--instance (audio-player-mpv)
  "Singleton mpv backend instance managing the mpv process and IPC connection.")

(defun audio-player-mpv--arguments (socket)
  "Return mpv arguments for SOCKET and media URL."
  (append audio-player-mpv-network-cache-args
          audio-player-mpv-extra-args
          (delq nil (list "--no-video"
                          "--idle"
                          (concat "--input-ipc-server=" socket)
                          "--pause=no"))))

(defun audio-player-mpv--connect (socket process attempt)
  "Connect to mpv SOCKET for PROCESS, retrying from ATTEMPT."
  (when (and (< attempt 40)
             (process-live-p process)
             (eq process (oref audio-player-mpv--instance process)))
    (condition-case err
        (let ((idx 1)
              (ipc (make-network-process
                    :name "audio-player-mpv-ipc"
                    :family 'local
                    :service socket
                    :coding 'utf-8
                    :noquery t
                    :filter #'audio-player-mpv--filter)))
          (process-put ipc 'pending "")
          (process-put ipc 'request-id 0)
          (process-put ipc 'callbacks (make-hash-table :test 'eql))
          (oset audio-player-mpv--instance ipc-process ipc)
          (dolist (event
                   '("pause" "core-idle" "time-pos" "duration" "playlist" "playlist-pos" "metadata"))
            (audio-player-mpv--send (list "observe_property" idx event))
            (setq idx (1+ idx))))
      (error
       (message "MPV IPC connection error (attempt %d): %S" attempt err)
       (run-at-time 0.05 nil
                    #'audio-player-mpv--connect socket process (1+ attempt))))))

(defun audio-player-mpv--current-ipc-p (process)
  "Return non-nil when PROCESS is the active mpv IPC connection."
  (eq process (oref audio-player-mpv--instance ipc-process)))

(defun audio-player-mpv--to-artist (data)
  "Extract artist name from mpv metadata DATA.
Returns the ARTIST value from the metadata hash table, or an empty string."
  (when data
    (or (gethash "ARTIST" data) "")))

(defun audio-player-mpv--event (event msg)
  "Mirror mpv EVENT and MSG into player state."
  (let ((data (gethash "data" msg)))
    (pcase event
      ("property-change"
       (pcase (gethash "name" msg)
         ("pause"
          (audio-player-update-status (if data 'paused 'playing)))
         ("core-idle"
          (when (not data)
            (audio-player-update-status 'playing)))
         ("time-pos"
          (audio-player-update-position data))
         ("duration"
          (audio-player-update-duration data))
         ("playlist"
          (audio-player-update-playlist (mapcar #'audio-player-mpv--to-playlist-item data)))
         ("playlist-pos"
          (audio-player-update-playlist-pos data))
         ("metadata"
          (audio-player-update-artist (audio-player-mpv--to-artist data))))
       (mapc #'funcall audio-player-update-hooks))
      ("end-file"
       (when (equal (gethash "reason" msg) "error")
         (message "Playback error: %s"
                  (or (gethash "file_error" msg) "unknown error")))))))

(defun audio-player-mpv--filter (process output)
  "Parse newline-delimited JSON OUTPUT from mpv PROCESS."
  (when (audio-player-mpv--current-ipc-p process)
    (let ((pending (concat (process-get process 'pending) output)))
      (while (string-match "\n" pending)
        (when-let* ((line-end (match-beginning 0))
                    (next-start (match-end 0))
                    (msg (audio-player-mpv--output-parse
                          process
                          (substring pending 0 line-end))))
          (audio-player-mpv--event (gethash "event" msg) msg)
          (when (and (process-get process 'callback)
                     (not (gethash "event" msg)))
            (funcall (process-get process 'callback) (gethash "data" msg)))
          (setq pending (substring pending next-start))))
      (process-put process 'pending pending))))

(defun audio-player-mpv--output-parse (process line)
  "Parse JSON LINE from mpv PROCESS.
Returns a hash table representation of the JSON, or nil on parse failure."
  (when (audio-player-mpv--current-ipc-p process)
    (ignore-errors
      (json-parse-string
       line
       :array-type 'list
       :null-object nil
       :false-object nil))))

(defun audio-player-mpv--ready-p ()
  "Return non-nil when the current mpv process can accept IPC commands."
  (let ((process (oref audio-player-mpv--instance process))
        (ipc-process (oref audio-player-mpv--instance ipc-process)))
    (and process
         ipc-process
         (process-live-p process)
         (audio-player-mpv-ipc-writable-p ipc-process))))

(defun audio-player-mpv--send (command &optional callback)
  "Send COMMAND to the current mpv IPC process."
  (when audio-player-debug-p
    (message (json-encode (list (cons 'command command)))))
  (when-let* ((ipc (oref audio-player-mpv--instance ipc-process))
              ((audio-player-mpv-ipc-writable-p ipc)))
    (condition-case nil
        (progn
          (process-put ipc 'callback callback)
          (process-send-string
           ipc
           (concat (json-encode (list (cons 'command command))) "\n"))
          t)
      (error
       (when (eq ipc (oref audio-player-mpv--instance ipc-process))
         (oset audio-player-mpv--instance ipc-process nil))
       nil))))

(defun audio-player-mpv--sentinel (process _event)
  "Advance when mpv PROCESS exits cleanly."
  (when (and (not (process-live-p process))
             (eq process (oref audio-player-mpv--instance process)))
    (if-let* ((exit-code (process-exit-status process)))
        (print "Exit"))))

(defun audio-player-mpv--start ()
  "Start the mpv process and establish an IPC connection.
Creates a Unix socket for IPC communication and starts mpv with appropriate
arguments.  Retries connection if mpv is not immediately ready."
  (unless (audio-player-mpv--ready-p)
    (when-let* ((socket (make-temp-name (file-name-concat temporary-file-directory "audio-player-mpv-")))
                (args (audio-player-mpv--arguments socket))
                (process (apply #'start-process "audio-player-mpv" nil audio-player-mpv-program args)))
      (print args)
      (set-process-sentinel process #'audio-player-mpv--sentinel)
      (oset audio-player-mpv--instance process process)
      (oset audio-player-mpv--instance socket socket)
      (audio-player-mpv--connect socket process 0))))

(defun audio-player-mpv--stop ()
  "Stop the mpv process and reset the backend instance.
Kills the live mpv process and replaces the singleton with a fresh instance."
  (when (audio-player-mpv--ready-p)
    (when-let* ((process (oref audio-player-mpv--instance process)))
      (kill-process process)
      (setq audio-player-mpv--instance (audio-player-mpv)))))

(defun audio-player-mpv--to-playlist-item (hash)
  "Convert mpv playlist HASH entry to an audio-player-playlist-item.
Extracts id, filename (as url), and title from the hash table."
  (audio-player-playlist-item
   :id (gethash "id" hash)
   :url (gethash "filename" hash)
   :title (or (gethash "title" hash)
              (file-name-base (gethash "filename" hash)))))

(defun audio-player-mpv-ipc-writable-p (ipc)
  "Return non-nil when IPC process can probably accept writes."
  (and (process-live-p ipc)
       (or (not (processp ipc))
           (memq (process-status ipc) '(open run)))))

(cl-defmethod audio-player-backend-add ((backend audio-player-mpv) url)
  "Add URL to the mpv playlist and begin playback."
  (audio-player-mpv--start)
  (audio-player-mpv--send (list "loadfile" url "append-play")))

(cl-defmethod audio-player-backend-next ((backend audio-player-mpv))
  "Skip to the next track in the mpv playlist."
  (audio-player-mpv--send (list "playlist-next")))

(cl-defmethod audio-player-backend-prev ((backend audio-player-mpv))
  "Go to the previous track in the mpv playlist."
  (audio-player-mpv--send (list "playlist-prev")))

(cl-defmethod audio-player-backend-repeat ((backend audio-player-mpv))
  "Toggle repeat mode.")

(cl-defmethod audio-player-backend-seek ((backend audio-player-mpv) seconds &optional mode)
  "Seek SECONDS in the current track using MODE (\"absolute\" or \"relative\")."
  (audio-player-mpv--send (list "seek" seconds mode)))

(cl-defmethod audio-player-backend-shuffle ((backend audio-player-mpv))
  "Toggle shuffle mode.")

(cl-defmethod audio-player-backend-start ((backend audio-player-mpv))
  "Start the mpv process and establish an IPC connection."
  (audio-player-mpv--start))

(cl-defmethod audio-player-backend-stop ((backend audio-player-mpv))
  "Stop the mpv process and reset the backend instance."
  (audio-player-mpv--stop))

(cl-defmethod audio-player-backend-toggle ((backend audio-player-mpv))
  "Toggle between playing and paused states."
  (audio-player-mpv--send (list "cycle" "pause")))

(cl-defmethod audio-player-backend-play-index ((backend audio-player-mpv) index)
  "Play the track at playlist position INDEX."
  (audio-player-mpv--send (list "playlist-play-index" index)))

(cl-defmethod audio-player-backend-remove-index ((backend audio-player-mpv) index)
  "Remove the track at playlist position INDEX."
  (audio-player-mpv--send (list "playlist-remove" index)))

(cl-defmethod audio-player-backend-ready-p ((backend audio-player-backend))
  "Return non-nil when the mpv backend is ready to accept IPC commands."
  (audio-player-mpv--ready-p))

(oset audio-player--instance backend audio-player-mpv--instance)

(provide 'audio-player-mpv)

;;; audio-player-mpv.el ends here
