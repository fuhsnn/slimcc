
fix_configure() {
 sed -i 's/^\s*lt_prog_compiler_wl=$/lt_prog_compiler_wl=-Wl,/g' "$1"
 sed -i 's/^\s*lt_prog_compiler_pic=$/lt_prog_compiler_pic=-fPIC/g' "$1"
 sed -i 's/^\s*lt_prog_compiler_static=$/lt_prog_compiler_static=-static/g' "$1"
}

replace_line() {
 sed -i s/^"$1"$/"$2"/g "$3"
}

github_tar() {
  mkdir -p "$2"
  curl -fL https://github.com/"$1"/"$2"/archive/refs/tags/"$3".tar.gz | tar xz -C "$2" --strip-components=1
  cd "$2"
}

github_clone() {
  git clone --depth 1 --recurse-submodules --shallow-submodules --branch "$3" https://github.com/"$1"/"$2"
  cd "$2"
}

git_fetch() {
 mkdir -p "$3" && cd "$3"
 git init
 git remote add origin "$1"
 git fetch --depth 1 origin "$2"
 git checkout FETCH_HEAD
}

url_tar() {
  mkdir -p "$2"
  curl -fL "$1" | tar xz -C "$2" --strip-components=1
  cd "$2"
}

install_libtool() {
 url_tar https://ftp.gnu.org/gnu/libtool/libtool-2.5.4.tar.gz __libtool
 fix_configure ./configure
 fix_configure libltdl/configure
 ./configure
 make -j2 install
 cd ../ && rm -rf __libtool
}
