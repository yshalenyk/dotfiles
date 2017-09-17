import os
import subprocess
from functools import partial
import logging

os.system = partial(subprocess.call, shell=True)
logging.basicConfig(level=logging.INFO)
FEDORA = '25'
PYTHONS = [
    '2.7.13',
    '3.6.2rc2',
    'pypy2.7-5.8.0',
    'pypy3.5-5.8.0'
]
ADDITIONAL_PKGS = [
    'https://releases.hashicorp.com/vagrant/1.9.5/vagrant_1.9.5_x86_64.rpm',
    'https://download.cdn.viber.com/desktop/Linux/viber.rpm'
]
INSTALL = [
    'openssl-devel',
    'xz',
    'xz-devel',
    'https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-{}.noarch.rpm'.format(FEDORA),
    'adobe-source-sans-pro-fonts',
    'adobe-source-serif-pro-fonts',
    'adobe-source-code-pro-fonts',
    'redhat-rpm-config',
    'python-devel',
    'python3-devel',
    'nodejs',
    'automake',
    'gcc',
    'gcc-c++',
    'kernel-devel',
    'cmake',
    'wget',
    'libffi',
    'libffi-devel',
    'vim',
    'ctags',
    'cmake',
    'emacs',
    'git',
    'tmux',
    'wget',
    'telegram-desktop',
    'zlib-devel',
    'redis',
    'python-devel',
    'zeromq-devel',
    'couchdb',
    'bzip2',
    'bzip2-devel',
    'readline-devel',
    'sqlite',
    'feh',
    'sqlite-devel',
    'vlc',
    'htop',
    'xclip',
    'curl',
    'joe',
    'jq'
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
    'kaddressbook-*'
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


def install_pythons():
    os.system('pyenv update')
    for python in PYTHONS:
        os.system('pyenv install {}'.format(python))


if __name__ == '__main__':
    install_packages()
    remove_packages()
    install_pyenv()
    for pks in ADDITIONAL_PKGS:
        os.system(CMD.format('install -y', pks))
    install_pythons()
