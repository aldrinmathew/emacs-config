# My `Emacs` config

- This is my `Emacs` configuration file. I like it when everything is in one file. I use and prefer `GNU Emacs` by the way.
- I have been using `VSCode` for as long as I have been a programmer. I have tried other IDEs along the way like Android Studio (mainly for Flutter and then Jetpack Compose), CLion (for qat compiler project), Vim, NeoVim, Helix...
- I have been fairly satisfied with `VSCode` for a long time, with some minor inconveniences. I tried have tried Emacs for a short time before and liked it, but switched back to `VSCode` as it had all the conveniences that I was used to. The positive experience with Emacs stuck with me for a while, until I got back into it by the end of August 2024. And I am loving it since, as I was able to replicate a reliable and functioning IDE setup. I have been missing this feeling for a long time - that is, the feeling of excitement from a technology or tool. Development sphere has been boring and useless for me for a long time - because of all the hype cycles. The only thing that kept me going was my language, `qat`. Now I have one more thing that I enjoy.
- What I hate about `vim`, `neovim` and `helix` the most, and what I think is the stupidest feature in the world, is that they all start in the non-edit mode. How stupid do you have to be, to make that the default mode? These are editors. The one thing people expect these "editors" to do without any hassle is to edit something!!
- My experiences with terminal editors like `vim`, `neovim` and `helix` has been pretty brief and straightforward - that those were pretty unimpressive environments to code in. Specifically, I have noticed the speed of neovim and helix and the flashiness these editors display to captivate its audiences. But I have not been able to achieve a stable, reliable setup while enjoying the process.
- `neovim` grabbed more of my attention due to its popularity among new-age enthusiasts, but the configuration and customization process turned out to be a mess. Something I believe many people have experienced - you eventually end up with a goliath configuration, half of which you have no understanding of, because you wanted a setup that works. I have also tried NvChad, and I guess it "works", but at some point, you find it tiring to get comfortable with a configuration that you had no say in creating.
- `helix` is in my opinion, meh. I like that the commands in practice seems to retain their names. Which makes commands obvious in a literate sense. However, Helix turned out to be a typical hyped-up `rust` project - the only interesting part of the project is that it is written in `rust`.
- `vim` or `vi` for that matter turned out to be boring and unintuitive. I will use it as many do, only when I login to an SSH server and have no other choice. It's useable though and has a unique place.

## Note

Things to do in a new machine

- Setup auto startup for `emacs --daemon`

- Set hotkey for emacs launch to be `emacsclient -c -e '(treemacs)' '(recentf-open-files)' --alternate-editor=`

- Set the wakatime API key and CLI path using `wakatime-prompt-api-key` & `wakatime-prompt-cli-path`

- Run `nerd-icons-install-fonts`. Required by `doom-modeline`

- Run `treesit-install-language-grammar` for tsx and use the repository path https://github.com/tree-sitter/tree-sitter-typescript
