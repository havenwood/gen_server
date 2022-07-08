# frozen_string_literal: true

require_relative 'gen_server/pid'
require_relative 'gen_server/registry'
require_relative 'gen_server/version'

module GenServer
  def init(state) = [:ok, state]
  def handle_cast(_message, _state) = [:noreply, nil]
  def handle_call(_message, _sender, _state) = [:reply, nil, nil]
  def terminate(_reason, _state) = [:stop]

  class << self
    def start_link(receiver, initial_state = [], name: nil)
      Object.const_set(name, receiver) if name

      receiver.init(initial_state) => [:ok, state]

      pid = PID.new

      server = Ractor.new(state, name: pid.to_s) do |state|
        GenServer.receive(state)
      end

      Registry[pid] = Registry::Info.new(server:, receiver:)

      [:ok, pid]
    end

    def receive(state)
      case Ractor.receive
      in [:cast, message, receiver]
        receiver.handle_cast(message, state) => [:noreply, new_state]
      in [:call, sender, message, receiver]
        receiver.handle_call(message, sender, state) => [:reply, reply, new_state]
        sender << [:ok, reply]
      in [:stop, reason, receiver]
        receiver.terminate(reason, state) => [:stop]
      end

      GenServer.receive(new_state)
    end

    def cast(pid, message)
      Registry.fetch(pid).values => [server, receiver]
      server << [:cast, message, receiver]

      :ok
    end

    def call(pid, message)
      Registry.fetch(pid).values => [server, receiver]
      server << [:call, Ractor.current, message, receiver]
      Ractor.receive => [:ok, response]

      response
    end

    def stop(pid, reason)
      Registry.fetch(pid).values => [server, receiver]
      server << [:stop, reason, receiver]
      server.close_outgoing
      Registry.delete(pid)

      :ok
    end
  end
end
