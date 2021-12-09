FROM haskell:8.10.7

WORKDIR /happ

COPY . .

CMD ["stack", "run"]
