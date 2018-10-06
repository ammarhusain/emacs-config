### Setup:
On Ubuntu

`sudo apt-get install emacs25 clang libclang-dev cmake clang-format`

On Mac OSX

`brew install emacs llvm cmake clang-format`



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
* After the initial setup, you can comment out the following line, to save time &
avoid an elpa package refresh on startup:

`(package-refresh-contents)`

* For Terminal setting on Mac:

      Preferences->Profiles->Keyboard

      Enable the "Use Option as Meta key"

* If you get this error:

     Company back-end 'company-clang' could not be initialized:

     Company found no clang executable

Symlink the clang versioned executable: `sudo ln -s /usr/bin/clang-3.8 /usr/bin/clang`

* Clang does not play well with GDB. More specifically it does not emit debug information for `libstdc++`.
This causes errors because GDB is unable to parse through STL datastructures like string, vector etc. Example error:

` Python Exception <class 'gdb.error'> There is no member named _M_dataplus.: `

There are 2 ways to solve this:
(a) Install the debug package for `libstdc++`: `sudo apt-get install libstdc++6-5-dbg`
(b) [Force Clang to emit debug symbols]
(https://stackoverflow.com/questions/41745527/cannot-view-stdstring-when-compiled-with-clang)
by setting the `-D_GLIBCXX_DEBUG` flag.
