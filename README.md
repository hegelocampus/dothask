# dothask

[![Hackage](https://img.shields.io/hackage/v/dothask.svg?logo=haskell)](https://hackage.haskell.org/package/dothask)
[![Stackage Lts](http://stackage.org/package/dothask/badge/lts)](http://stackage.org/lts/package/dothask)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Dotfile setup automation written in Haskell

## How to use:
Run the tool in the directory containing your dotfiles. E.g., if your dotfiles directory is `$HOME/.dotfiles` you should cd into `$HOME/.dotfiles`.

You need to create a `dot.config.yaml` file in your dotfile directory.

## Configuration
DotHask uses YAML-formatted configuration files to declare which files need to be **linked**, which directories need to be **created**, and which **shell commands** should be ran.
The expected name for the configuration file is `dot.config.yaml`. You can pass in a custom filepath using `dothask -c <path to config file>`.

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
Link specifies how files and directories should be linked. You may specify a file be forcibly linked, overwriting existing files if they exist.
##### Format
Available config parameters:
| Link Option | Explanation |
| -- | -- |
| `path` | The source file for the symlink (default: null, automatic) |
| `create` | When true, create parent directories to the link as needed (default: false) |
| `relink` | Removes the old target if it's a symlink (default: false) |
| `force` | Force removes the old target (default: false) |
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
Copyright (c) 2020 Bee Ellis. Released under the MIT License. See [LICENSE.md][license] for details.
