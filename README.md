# dothask

[![Hackage](https://img.shields.io/hackage/v/dothask.svg?logo=haskell)](https://hackage.haskell.org/package/dothask)
[![Stackage Lts](http://stackage.org/package/dothask/badge/lts)](http://stackage.org/lts/package/dothask)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Dotfile setup automation written in Haskell. Heavily inspired by @anishathalye's [dotbot](https://github.com/anishathalye/dotbot).

## Instalation:
Clone this repository and install dothask with the following commands: 
```bash
$ git clone https://github.com/hegelocampus/dothask.git && cd dothask
$ stack install
```

You then need to create a `dot.config.yaml` file in your dotfile directory. You can use the [`example.config.yaml`](example.config.yaml) as a starting place if you'd like.

## How to use:
Run the tool in the directory containing your dotfiles. E.g., if your dotfiles directory is `~/.dotfiles` you should cd into `~/.dotfiles` before running `stack exec dothask`.

## Configuration
Dothask uses YAML-formatted configuration files to declare which files need to be **linked**, which directories need to be **created**, and which **shell commands** should be ran.
The expected name for the configuration file is `dot.config.yaml`. You can pass in a custom filepath using `dothask -c <path to config file>`.  
In addition, you may pass the `--no-confirm` flag to override the default file overwite behavior, which will ask you before relpacing any (non symlink) file. Note that this is the behavior even when the link configuration is set to `force`. **The force flag simply allows for any overwrite to happen at all.**

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

## Not yet implemented
create:
  - ~/downloads
  - ~/.vim/undo-history

## Not yet implemented
shell:
  - [git submodule update --init --recursive, Installing submodules]
```
### Configuration options
#### Link
Link specifies how files and directories should be linked. You may specify a file be forcibly linked, overwriting existing files if they exist, but be aware that **the default behavior for force will still require confirmation before removing normal (non-symlink) files.** If you'd like to override this default behavior and make the stript fully automatic you can pass in the `--no-confirm` flag when you run the program.
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
    path: vim/vimrc
```
#### Create -- Not yet implemented
Create commands specify directories to be created.
##### Format
Specified as an array of directories.
##### Example
```yaml
```
## License
Copyright (c) 2020 Bee Ellis. Released under the MIT License. See [LICENSE.md](license) for details.
