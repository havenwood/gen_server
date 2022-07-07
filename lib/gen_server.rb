# frozen_string_literal: true

require_relative 'gen_server/pid'
require_relative 'gen_server/registry'
require_relative 'gen_server/version'

module GenServer
  def init(state) = [:ok, state]
  def handle_cast(...) = [:noreply, nil]
  def handle_call(...) = [:reply, nil, nil]

  class << self
    def start_link(receiver, initial_state = [])
      receiver.init(initial_state) => [:ok, state]

      pid = PID.new

      actor = Ractor.new(state, name: pid.to_s) do |state|
        GenServer.receive(state)
      end

      Registry[pid] = Registry::Info.new(actor:, receiver:)

      [:ok, pid]
    end

    def receive(state)
      case Ractor.receive
      in [:cast, message, receiver]
        receiver.handle_cast(message, state) => [:noreply, new_state]
      in [:call, sender, message, receiver]
        receiver.handle_call(message, sender, state) => [:reply, reply, new_state]
        sender.send [:ok, reply]
      end

      receive(new_state)
    end

    def cast(pid, message)
      Registry.fetch(pid).values => [actor, receiver]
      actor.send [:cast, message, receiver]

      :ok
    end

    def call(pid, message)
      Registry.fetch(pid).values => [actor, receiver]
      actor.send [:call, Ractor.current, message, receiver]
      Ractor.receive => [:ok, response]

      response
    end
  end
end
