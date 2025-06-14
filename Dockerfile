FROM erlang:26-alpine AS build

RUN apk --update add git gcc make g++ zlib-dev

RUN mkdir /buildroot
WORKDIR /buildroot

COPY auctioneer auctioneer
WORKDIR auctioneer

RUN rebar3 as prod release

# ---------------------------------------------------------------

FROM alpine

RUN mkdir /auctioneer
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

COPY --from=build /buildroot/auctioneer/_build/prod/rel/auctioneer /auctioneer

EXPOSE 9000

CMD ["/auctioneer/bin/auctioneer", "foreground"]