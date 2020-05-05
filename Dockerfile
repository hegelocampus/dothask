FROM haskell:latest as build

WORKDIR /app/build/

RUN cabal update

COPY ./dothask.cabal /app/build/dothask.cabal

##RUN cabal install exe:dothask --only-dependencies -j4

COPY . /app/build/
RUN cabal install exe:dothask -v0 -j4

RUN cabal build exe:dothask -g --builddir=.

FROM archlinux:latest

RUN pacman -Syyuq --noconfirm && pacman -S zsh git xdg-user-dirs --noconfirm
RUN useradd -m test && xdg-user-dirs-update

WORKDIR /home/test/

RUN git clone https://github.com/hegelocampus/.dotfiles

WORKDIR /home/test/.dotfiles
COPY --from=build /app/build/./build/x86_64-linux/ghc-8.8.3/dothask-0.0.0.0/x/dothask/build/dothask/dothask /home/test/.dotfiles/
COPY --from=build /app/build/dot.config.yaml /home/test/.dotfiles/

CMD ["./dothask", "dot.config.yaml"]

