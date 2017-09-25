PATH=$PATH:$HOME/.local/bin:$HOME/bin
PATH="/home/yshalenyk/.pyenv/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"


eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
pyenv global 2.7.13


export RUST_SRC_PATH=~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
