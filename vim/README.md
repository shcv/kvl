# KVL Vim and Neovim Support

This directory provides the runtime files needed for `*.kvl` editing:

- `ftdetect/kvl.vim`
- `ftplugin/kvl.vim`
- `indent/kvl.vim`
- `syntax/kvl.vim`

## Install

Copy or symlink the `vim/` tree into your Vim or Neovim runtime path. For a
pack-based install:

```sh
mkdir -p ~/.local/share/nvim/site/pack/kvl/start
ln -s /path/to/kvl/vim ~/.local/share/nvim/site/pack/kvl/start/kvl
```

For Vim, use the matching `~/.vim/pack/...` path.

## Use

Open any `*.kvl` file. The filetype detector sets `filetype=kvl`, which loads
the syntax, indent, and ftplugin files automatically.

If you are testing manually, this is enough to force the runtime files:

```vim
:set rtp+=/path/to/kvl/vim
:filetype plugin indent on
```
