#!/usr/bin/sh
setcap 'cap_net_bind_service=+ep' /usr/lib/erlang/erts-5.8.1/bin/beam
setcap 'cap_net_bind_service=+ep' /usr/lib/erlang/erts-5.8.4/bin/beam.smp
