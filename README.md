### Setup:
`sudo apt-get install emacs25 clang cmake clang-format libclang-dev`

Then start:

`emacs --debug--init`

`M-x irony-install-server`


### Features:
* **company** for autocompletion
* **irony-mode** for C++ intellisense, using libclang under the hood
* **dirtree** for filesystem browsing
* **clang-format** for code styling & indenting
* **flycheck** to highlight warnings/errors based on **irony-mode**
* **doxymacs** to help autogenerate doxygen documentation blocks
* **ivy** & **counsel** for a completion frontend; makes it easy to search through buffers
* Several custom keybindings & functions for efficiency/productivity
* **fireplace** for a cozy fireplace animation


### Notes:
* Need compilation database for irony mode & flycheck to work well.
  `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..`
* No jump to definition: instead use `Ctrl-C Ctrl-G` to grep within git repo
* For Terminal setting on Mac:

      Preferences->Profiles->Keyboard

      Enable the "Use Option as Meta key"
