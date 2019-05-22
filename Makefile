PROJECT = auctions
PROJECT_DESCRIPTION = Module for controlling and spawning auctions
PROJECT_VERSION = 0.1.0


DEPS = cowboy ejwt hackney uuid jiffy
dep_cowboy_commit = 2.6.3

DEP_PLUGINS = cowboy



include erlang.mk
