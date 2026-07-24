;;; audio-player-backend-api.el ---                  -*- lexical-binding: t; -*-

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

(defclass audio-player-backend ()
  ()
  "Abstract base class for audio player backends.
Backends must implement the `audio-player-backend-add',
`audio-player-backend-toggle', `audio-player-backend-next',
`audio-player-backend-prev', `audio-player-backend-seek',
`audio-player-backend-start', `audio-player-backend-stop', and
`audio-player-backend-play-index' methods.")

(cl-defmethod audio-player-backend-add ((backend audio-player-backend) url)
  "Add URL to the backend's playlist and begin playback.")

(cl-defmethod audio-player-backend-next ((backend audio-player-backend))
  "Skip to the next track in the playlist.")

(cl-defmethod audio-player-backend-play-index ((backend audio-player-backend) index)
  "Play the track at playlist position INDEX.")

(cl-defmethod audio-player-backend-prev ((backend audio-player-backend))
  "Go to the previous track in the playlist.")

(cl-defmethod audio-player-backend-ready-p ((backend audio-player-backend))
  "Return non-nil when the backend is ready to accept commands.")

(cl-defmethod audio-player-backend-remove-index ((backend audio-player-backend) index)
  "Remove the track at PLAYLIST-POS from the backend's playlist.")

(cl-defmethod audio-player-backend-repeat ((backend audio-player-backend))
  "Toggle repeat mode.")

(cl-defmethod audio-player-backend-seek ((backend audio-player-backend) seconds mode)
  "Seek SECONDS in the current track.
MODE should be \"absolute\" for an exact position or \"relative\" for an offset.")

(cl-defmethod audio-player-backend-shuffle ((backend audio-player-backend))
  "Toggle shuffle mode.")

(cl-defmethod audio-player-backend-start ((backend audio-player-backend))
  "Start the backend process and prepare for playback.")

(cl-defmethod audio-player-backend-stop ((backend audio-player-backend))
  "Stop playback and shut down the backend process.")

(cl-defmethod audio-player-backend-toggle ((backend audio-player-backend))
  "Toggle between playing and paused states.")

(provide 'audio-player-backend-api)
;;; audio-player-backend-api.el ends here
