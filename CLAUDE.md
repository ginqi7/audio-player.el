# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

```sh
make test              # Install dependencies and run ERT tests
make solve-dependencies # Clone dependencies to ~/.emacs.d/lisp/
```

Run tests with a specific Emacs version via CI (tests run on Ubuntu with Emacs 28.1, 29.1, 30.1, release-snapshot, snapshot).

## Architecture

### Layered Design

The project follows a layered architecture with clear separation of concerns:

1. **Core** (`audio-player.el`) — Player state, playlist management, user commands via Transient
2. **Backend API** (`audio-player-backend-api.el`) — Abstract interface defining playback operations
3. **Backend Implementation** (`audio-player-mpv.el`) — mpv process management via socket IPC
4. **UI** (`audio-player-ui.el`) — Child frame display with VUI component rendering

### Backend Interface Pattern

Backends implement `audio-player-backend` EIEIO class. Each backend subclass must implement:
- `audio-player-backend-add` — add media (file or URL)
- `audio-player-backend-toggle` — play/pause
- `audio-player-backend-start` / `audio-player-backend-stop` — lifecycle
- `audio-player-backend-next` / `audio-player-backend-prev` — playlist navigation
- `audio-player-backend-seek` — with seconds and mode ("absolute" or "relative")
- `audio-player-backend-play-index` — play by playlist index

The mpv backend communicates over a Unix socket using JSON-encoded commands and observes mpv events (`pause`, `time-pos`, `duration`, `playlist`, `playlist-pos`, `metadata`).

### State Flow

```
User command → audio-player.el → backend method → mpv process
mpv event → audio-player-mpv--filter → audio-player-update-* functions → audio-player-update-hooks → UI refresh
```

Player state lives in `audio-player--instance` (an `audio-player` object): playlist, playlist position, status (`playing`/`paused`), repeat, shuffle.

To hook into state changes, add a function to `audio-player-update-hooks`. The UI does this at line 396 of `audio-player-ui.el`.

### UI Rendering

UI uses VUI (vui-component, vui-defcomponent) to build a component tree. The component `audio-player` renders: title → artist → progress bar with position/duration → playback controls. Display is via a non-focusable child frame positioned in the lower-right corner of the parent frame.

## Project Structure

- **Main package file**: `audio-player.el` — provides the `audio-player` symbol
- **Backend API**: `audio-player-backend-api.el` — abstract class and method signatures
- **mpv backend**: `audio-player-mpv.el` — binds `audio-player--instance` backend slot to mpv singleton
- **UI module**: `audio-player-ui.el` — requires `audio-player`, not the backend directly
- **Tests**: `tests/tests.el` — ERT tests, loads all `.el` files in tests directory

## Dependency Management

Dependencies are git repositories cloned to `~/.emacs.d/lisp/`. The `dependencies.sh` script parses `dependencies.txt` (one URL per line). Tests add dependencies to `load-path` via `load-dependencies-path` helper in `tests.el`.
