# Supported Window Managers
- yabai

# Demos
TODO

# Installation
TODO

# Quick Start
The set up required depends on your window manager

## Yabai

### Prerequisites
- yabai
- jq
- emacs (with a daemon currently running)

### Setup

#### Startup - (put these in your shell's profile e.g. .bash_profile, .bashrc, etc)
- Tell yabai to auto-focus emacs-clip when it gets instantiated
```shell
yabai -m signal --add event=window_created title='^emacs-clippo.*' action='yabai -m window --focus ${YABAI_WINDOW_ID}'
```

- Tell yabai configure emacs-clip to be a floating window
``` shell
yabai -m rule --add title='^emacs-clippo.*' layer=above manage=off
```

#### Command
- Command to invoke (you might want to create a keybinding for this, e.g. via skhd)
``` shell
emacsclient --eval "(clipboard-yabai $(yabai -m query --windows --window | jq .id))"
```
We need to pass in the window ID, otherwise, OSX fuses Emacs GUI (from which emacsclient was spawned) after you copy to clipboard
