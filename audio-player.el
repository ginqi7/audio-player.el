;;; audio-player.el ---                              -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'audio-player-backend-api)

(defvar audio-player-update-hooks nil
  "Hook run after any player state update.
Each function is called with no arguments after state changes like
position, duration, playlist, status, etc.")

(defcustom audio-player-seek-step 30
  "Default number of seconds to seek forward or backward.")

(defclass audio-player-playlist-item ()
  ((id :initarg :id :initform nil
       :documentation "Unique identifier for this playlist item.")
   (title :initarg :title :initform nil
          :documentation "Display title of the audio track.")
   (url :initarg :url :initform nil
        :documentation "File path or URL of the audio.")
   (duration :initarg :duration :initform 0
             :documentation "Total duration in seconds.")
   (position :initarg :position :initform 0
             :documentation "Current playback position in seconds.")
   (artist :initarg :artist :initform nil
           :documentation "Artist name extracted from metadata.")
   (cover-url :initarg :cover-url
              :documentation "URL of the cover art image.")))

(defclass audio-player ()
  ((backend :initarg :backend :initform nil
             :documentation "Backend instance (e.g., audio-player-mpv) handling playback.")
   (playlist :initarg :playlist :initform nil
             :documentation "List of audio-player-playlist-item objects.")
   (playlist-pos :initarg :playlist-pos :initform 0
                 :documentation "Current position index in the playlist.")
   (status :initarg :status :initform nil
           :documentation "Playback status, either 'playing or 'paused.")
   (repeat :initarg :repeat :initform nil
           :documentation "Non-nil means repeat mode is enabled.")
   (shuffle :initarg :shuffle :initform nil
            :documentation "Non-nil means shuffle mode is enabled.")))

(defvar audio-player--instance (audio-player)
  "Ephemeral player state for mpv processes and sockets.")

(defun audio-player-update-status (status)
  "Update the player's playback status to STATUS.
STATUS should be 'playing or 'paused."
  (oset audio-player--instance status status))

(defun audio-player-update-artist (artist)
  "Update the artist name of the currently playing track to ARTIST."
  (when-let* ((playing-item (audio-player-current-playing)))
    (oset playing-item artist artist)))

(defun audio-player-current-playing ()
  "Return the currently playing audio-player-playlist-item, or nil if none."
  (when-let* ((playlist (oref audio-player--instance playlist))
              (playlist-pos (oref audio-player--instance playlist-pos)))
    (nth playlist-pos playlist)))

(defun audio-player-update-position (position)
  "Update the current playback position to POSITION seconds."
  (when-let* ((playing-item (audio-player-current-playing)))
    (oset playing-item position position)))

(defun audio-player-update-duration (duration)
  "Update the total duration of the current track to DURATION seconds."
  (when-let* ((playing-item (audio-player-current-playing)))
    (when duration
      (oset playing-item duration duration))))

(defun audio-player-update-playlist-item (old new)
  "Merge metadata from NEW into existing playlist item OLD.
Only copies id, title, and artist properties when OLD lacks them."
  (when new
    (dolist (prop '(id title artist))
      (when (and (not (eieio-oref old prop))
                 (eieio-oref new prop))
        (eieio-oset old prop (eieio-oref new prop))))
    old))

(defun audio-player-update-playlist (playlist)
  "Merge new playlist items into the existing playlist.
Updates metadata (title, artist, etc.) for existing items by URL."
  (let ((playlist-hash (make-hash-table :test #'equal))
        (current-playlist (oref audio-player--instance playlist)))
    (mapc (lambda (item) (puthash (oref item url) item playlist-hash)) playlist)
    (oset audio-player--instance playlist
          (remove nil (mapcar (lambda (item)
                                (audio-player-update-playlist-item
                                 item
                                 (gethash (oref item url) playlist-hash)))
                              current-playlist)))))

(defun audio-player-update-playlist-pos (playlist-pos)
  "Update the current playlist position to PLAYLIST-POS."
  (oset audio-player--instance playlist-pos playlist-pos))

(defun audio-player-toggle()
  "Toggle playback between playing and paused states."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-toggle backend)))

(defun audio-player-seek(seconds)
  "Seek to absolute position SECONDS in the current track."
  (interactive "nSeconds: ")
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-seek backend seconds "absolute")))

(defun audio-player--audio-file-p (file)
  "Check if FILE is an audio file using mailcap."
  (when-let* ((extension (file-name-extension file))
              (type (mailcap-extension-to-mime extension)))
    (string-prefix-p "audio/" type)))

(defun audio-player-add-directory (directory)
  "Add all audio files from DIRECTORY to the playlist recursively."
  (interactive "DSelect an audio directory ")
  (mapcar #'audio-player-add-file
          (seq-filter #'audio-player--audio-file-p (directory-files directory t))))

(cl-defmethod audio-player-add-playlist-item ((item audio-player-playlist-item))
  (let ((playlist (oref audio-player--instance playlist)))
    (oset audio-player--instance playlist (append playlist (list item))))
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-add backend (oref item url))))

(defun audio-player-add-file (file)
  "Add FILE to the playlist and start playback.
FILE should be a valid audio file path."
  (interactive "fSelect an audio file: ")
  (audio-player-add-playlist-item (audio-player-playlist-item :url file)))

(defun audio-player-add-url (url)
  "Add URL to the playlist and start playback.
URL should be a valid audio URL (http, https, etc.)."
  (interactive "sURL: ")
  (audio-player-add-playlist-item (audio-player-playlist-item :url url)))

(defun audio-player-next ()
  "Skip to the next track in the playlist."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-next backend)))

(defun audio-player-previous ()
  "Go to the previous track in the playlist."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-prev backend)))

(defun audio-player-seek-forward ()
  "Seek forward by `audio-player-seek-step' seconds."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-seek backend audio-player-seek-step "relative")))

(defun audio-player-seek-previous ()
  "Seek backward by `audio-player-seek-step' seconds."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-seek backend (- 0 audio-player-seek-step) "relative")))

(defun audio-player--playlist-item-format (item)
  "Return a formatted string representation of playlist ITEM."
  (format "[%s] %s (%s)"
          (oref item id)
          (oref item title)
          (oref item artist)))

(defun audio-player-select-in-playlist (playlist)
  "Prompt the user to select an item from PLAYLIST and return its index."
  (let ((selected-str (completing-read
                       "Select an audio in Playlist: "
                       (mapcar #'audio-player--playlist-item-format playlist))))
    (cl-position-if
     (lambda (item) (string= selected-str (audio-player--playlist-item-format item)))
     playlist)))

(defun audio-player-find (&optional index)
  "Find and play a track from the current playlist.
With INDEX, play the track at that position directly.
Otherwise, prompt the user to select a track interactively."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend))
              (playlist (oref audio-player--instance playlist)))
    (audio-player-index
     (or index
         (audio-player-select-in-playlist playlist)))))

(defun audio-player-remove (&optional index)
  "Remove a track from the playlist.
With INDEX, remove the track at that position.
Otherwise, prompt the user to select a track to remove."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend))
              (playlist (oref audio-player--instance playlist)))
    (audio-player-remove-index
     (or index
         (audio-player-select-in-playlist playlist)))))

(defun audio-player-remove-index (index)
  "Remove the track at PLAYLIST-POS from the backend's playlist."
  (interactive "nPlease input index of playlist: ")
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-remove-index backend index)))

(defun audio-player-index (index)
  "Play the track at playlist position INDEX."
  (interactive "nPlease input index of playlist: ")
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-play-index backend index)))

(defun audio-player-start ()
  "Start the audio player and backend process."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-start backend)))

(defun audio-player-stop ()
  "Stop playback and terminate the backend process."
  (interactive)
  (when-let* ((backend (oref audio-player--instance backend)))
    (audio-player-backend-stop backend)
    (setq audio-player--instance (audio-player :backend backend))))

(defun audio-player-restart ()
  "Stop and restart the audio player backend."
  (interactive)
  (audio-player-stop)
  (audio-player-start))

(transient-define-prefix audio-player-menu ()
  [:description
   (lambda () (format "Audio Player Menu. Backend [%s]
There are [%s] songs
current [%s] is (%s) [%s]"
                  (audio-player-backend-ready-p (oref audio-player--instance backend))
                  (length (oref audio-player--instance playlist))
                  (oref audio-player--instance status)
                  (oref audio-player--instance playlist-pos)
                  (when (audio-player-current-playing)
                    (oref (audio-player-current-playing) title))))

   [""
    ("r" "Restart" audio-player-restart)
    ("SPC" "Play / Pause" audio-player-toggle)]
   ["Playlist"
    ("au" "Open URL" audio-player-add-url)
    ("af" "Open File" audio-player-add-file)
    ("ad" "Open Directory" audio-player-add-directory)
    ("d" "Remove" audio-player-remove)]
   ["Navigation"
    ("s" "Seek to" audio-player-seek)
    ("," audio-player-seek-previous
     :description (lambda () (format "Prev %s seconds" audio-player-seek-step))
     :transient t)
    ("."  audio-player-seek-forward
     :description (lambda () (format "Next %s seconds" audio-player-seek-step))
     :transient t)]
   [ "Playlist Select"
     ("f" "Find" audio-player-find)
     ("i" "Index" audio-player-index)
     ("<" "Prev Audio" audio-player-previous)
     (">" "Next Audio" audio-player-next)]])

(provide 'audio-player)

;;; audio-player.el ends here
