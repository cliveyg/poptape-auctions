PROJECT = auctioneer
PROJECT_DESCRIPTION = A microservice for controlling auctions
PROJECT_VERSION = 0.1.0


DEPS = cowboy ejwt hackney uuid jiffy amqp_client
dep_cowboy_commit = 2.6.3

DEP_PLUGINS = cowboy



include erlang.mk
