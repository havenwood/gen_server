# frozen_string_literal: true

require_relative '../lib/gen_server'

class Noop
  include GenServer
end

GenServer.start_link(Noop, []) => [:ok, pid]
p GenServer.call(pid, :foo)
p GenServer.cast(pid, %i[push bar])
p GenServer.call(pid, :foo)
