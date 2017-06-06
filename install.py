import os
import logging


logging.basicConfig(level=logging.INFO)
ADDITIONAL_PKGS = [
    'https://releases.hashicorp.com/vagrant/1.9.5/vagrant_1.9.5_x86_64.rpm',
    'https://download.cdn.viber.com/desktop/Linux/viber.rpm'
]
INSTALL = [
    'adobe-source-sans-pro-fonts',
    'adobe-source-serif-pro-fonts',
    'adobe-source-code-pro-fonts',
    'wget',
    'vim',
    'emacs',
    'git',
    'tmux',
    'wget',
    'telegram-desktop',
    'telegram-desktop',
    'zlib-devel',
    'bzip2',
    'bzip2-devel',
    'readline-devel',
    'sqlite',
    'sqlite-devel',
    'sqlite-devel',
    'vlc',
    'htop',
    'xclip',
    'curl',
    'joe'
]
REMOVE = [
    'korganizer',
    'kmail',
    'akregator',
    'konversation',
    'calligra-*',
    'kb',
    'kget',
    'konqueror',
    'ktp-*',
    'qupzilla',
]
CMD = 'sudo dnf {} {}'


def install_packages():
    for pkg in INSTALL:
        os.system(CMD.format('install -y', pkg))


def remove_packages():
    for pkg in REMOVE:
        os.system(CMD.format('remove -y', pkg))


def install_pyenv():
    url = 'https://raw.githubusercontent.com/pyenv/pyenv-installer/master/bin/pyenv-installer'
    os.system('curl -L {}|bash'.format(url))


def install_additional_pkgs():
    """TODO: """

    
if __name__ == '__main__':
    install_packages()
    remove_packages()
    install_pyenv()
