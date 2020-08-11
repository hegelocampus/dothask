# dothask

[![Hackage](https://img.shields.io/hackage/v/dothask.svg?logo=haskell)](https://hackage.haskell.org/package/dothask)
[![Stackage Lts](http://stackage.org/package/dothask/badge/lts)](http://stackage.org/lts/package/dothask)
![Tests](https://github.com/hegelocampus/dothask/workflows/Haskell%20CI/badge.svg)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Dotfile setup automation written in Haskell. Heavily inspired by @anishathalye's [dotbot](https://github.com/anishathalye/dotbot).

## Installation:
Clone this repository and install dothask with the following commands: 
```bash
$ git clone https://github.com/hegelocampus/dothask.git && cd dothask
$ stack install
```

You then need to create a `dot.config.yaml` file in your dotfile directory. You can use the [`example.config.yaml`](example.config.yaml) as a starting place if you'd like.

## How to use:
Run the tool in the directory containing your dotfiles. E.g., if your dotfiles directory is `~/.dotfiles` you should do something like this:
```bash
$ cd ~/.dotfiles
$ stack exec dothask
```

## Configuration
Dothask uses YAML-formatted configuration files to declare which files need to be **linked**, which directories need to be **created**, and which **shell commands** should be ran.
The expected name for the configuration file is `dot.config.yaml`. You can pass in a custom filepath using `dothask -c <path to config file>`.  
In addition, you may pass the `--no-confirm` flag to override the default file overwrite behavior, which will ask you before replacing any (non-symlink) file. Note that this is the behavior even when the link configuration is set to `force`. **The force flag simply allows for any overwrite to happen at all.**

### Example Configuration file
```yaml
defaults:
    link:
        relink: true

link:
    ~/.dotfiles: ''
    ~/.tmux.conf: tmux.conf
    ~/.vim: vim
    ~/.vimrc: vimrc

create:
    - ~/downloads
    - ~/.vim/undo-history

shell:
    - git submodule update --init --recursive
```
### Configuration options
#### Link
Link specifies how files and directories should be linked. You may specify a file be forcibly linked, overwriting existing files if they exist, but be aware that **the default behavior for force will still require confirmation before removing normal (non-symlink) files.** If you'd like to override this default behavior and make the script fully automatic you can pass in the `--no-confirm` flag when you run the program.  
##### Format
Available config parameters:
| Link Option | Explanation |
| -- | -- |
| `path` | The source file for the symlink (default: null, automatic) |
| `create` | When true, create parent directories to the link as needed (default: false) |
| `relink` | Removes the old target if it's a symlink (default: false) |
| `force` | Permits removal of the old target (default: false) |
##### Example
```yaml
link:
    ~/.vimrc:
        relink: true
        path: vim/
```
##### path
If you don't declare a source path, the default behavior is to link from the given link name to the file with the same name except without a leading "." in your current directory. For example, given you are in `~/.dotfiles`, the following config will link to `~/.dotfiles/vimrc`.
```yaml
link:
    ~/.vimrc:
```
You may also give `path` a directory, in that case a link will be created from the link path to the given directory in your current working directory. For example, again given you are in `~/.dotfiles`, the following config will link to `~/.dotfiles/vim/vimrc`. **Your path must have a trailing slash for dothask to know its a directory, otherwise it will assume you're giving a regular filename**
```yaml
link:
    ~/.vimrc:
        path: vim/
```
#### Dirs
`dir` specifies directories to be created. 
##### Format
Directories should be specified within a `yaml` list. Unlike directories in `link`, these don't need a trailing `/`, **dothask will assume anything specified in `dir` should be a directory**
##### Example
```yaml
dir:
    - ~/downloads
    - ~/usbmount
```
#### Shell
`shell` commands specify directories to be created. 
##### Format
Shell commands should be specified within a `yaml` list, witch each entry being the full shell script to run.
##### Example
```yaml
shell:
    - git submodule update --init --recursive
    - "git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si && cd .."
```
## License
Copyright (c) 2020 Bee Ellis. Released under the MIT License. See [LICENSE.md](license) for details.
