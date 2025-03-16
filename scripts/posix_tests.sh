test_pdpmake() {
 git_fetch https://github.com/rmyorston/pdpmake 7adf57a0171bf188ae14576223715215a06d5768 pdpmake
 make test
}

test_qbe_hare() {
 git_fetch git://c9x.me/qbe.git 903610de4fb27ad020e30b0e77eb0a278a949524 qbe
 make CC="$CC" check

 OS=`uname | tr '[:upper:]' '[:lower:]'`

 if [ $OS = linux ] || [ $OS = freebsd ] || [ $OS = netbsd ] || [ $OS = openbsd ]; then
  url_tar https://git.sr.ht/~sircmpwn/harec/archive/0.24.2.tar.gz harec
  mv configs/"$OS".mk config.mk
  make CC="$CC" QBE=../qbe check
 fi
}

test_zlib() {
 github_tar madler zlib v1.3.1
 CFLAGS=-fPIC ./configure
 make test
}

build_oksh() {
 git_fetch https://github.com/ibara/oksh 2484299d0c295607a0b6890be9b26b5710ab48f1 oksh
 ./configure
 make
}
