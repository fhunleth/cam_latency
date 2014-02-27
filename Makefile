PROJECT = cam_latency

DEPS = cowboy erlang-serial
dep_cowboy = pkg://cowboy master
dep_erlang-serial = https://github.com/knewter/erlang-serial.git

include erlang.mk
