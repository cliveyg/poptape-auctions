FROM erlang:27-alpine AS build

RUN apk --update add git gcc make g++ zlib-dev

RUN mkdir /buildroot
WORKDIR /buildroot

COPY auctioneer auctioneer
WORKDIR auctioneer

RUN rm -rf _build
RUN rebar3 as prod release

# ---------------------------------------------------------------

FROM alpine

RUN mkdir /auctioneer
RUN mkdir /auctioneer/log
RUN touch /auctioneer/log/poptape_auctioneer.log
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache bash && \
    apk add --no-cache curl && \
    apk add --no-cache libstdc++

COPY --from=build /buildroot/auctioneer/_build/prod/rel/auctioneer /auctioneer

EXPOSE $PORT

CMD ["/auctioneer/bin/auctioneer", "foreground"]