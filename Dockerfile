FROM haskell:latest

WORKDIR /home/bee/.dotfiles

RUN cabal update

COPY ./dothask.cabal /home/bee/.dotfiles/dothask.cabal

##RUN cabal install --only-dependencies -v3 -j4

COPY . /home/bee/.dotfiles/
RUN cabal install

RUN cabal build

CMD ["./dothask", "dot.config.yaml"]

