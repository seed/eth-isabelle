FROM archlinux/base
RUN pacman --noconfirm -Sy wget
RUN useradd --create-home app
USER app
ENV HOME=/home/app
WORKDIR $HOME
ENV PATH=$PATH:$HOME/bin
RUN mkdir bin
RUN wget -O bin/solc https://github.com/ethereum/solidity/releases/download/v0.5.4/solc-static-linux
RUN chmod a+x bin/solc
WORKDIR $HOME/erc725
CMD solc --allow-paths /home/app/erc725/ --bin --bin-runtime --asm --hashes -o res --overwrite Identity.sol
