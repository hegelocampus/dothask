FROM haskell:latest as build

WORKDIR /app/build/

RUN cabal update
RUN stack upgrade

COPY dothask.cabal /app/build/dothask.cabal
COPY stack.yaml /app/build/stack.yaml

RUN rm -rf ~/.stack && \
    stack config set system-ghc --global true && \
    stack setup && \
    stack install --split-objs --ghc-options="fPIC -fllvm" --only-dependencies

COPY . /app/build/
RUN stack build dothask -j 9

FROM archlinux:latest

RUN pacman -Syyuq --noconfirm && pacman -S zsh git xdg-user-dirs vim --noconfirm

RUN useradd -ms /bin/zsh test && xdg-user-dirs-update
USER test
WORKDIR /home/test/

RUN git clone https://github.com/hegelocampus/.dotfiles

WORKDIR /home/test/.dotfiles

COPY --from=build \
  /app/build/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/dothask/dothask \
  /home/test/.dotfiles/
COPY --from=build /app/build/dot.config.yaml /home/test/.dotfiles/

CMD ["./dothask", "dot.config.yaml"]

