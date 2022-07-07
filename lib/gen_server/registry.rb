# frozen_string_literal: true

require_relative 'registry/info'

module GenServer
  class Registry
    include Singleton

    attr_reader :pids

    def initialize
      @pids = {}
    end

    class << self
      def []=(pid, info)
        instance.pids[pid] = info
      end

      def actor(pid)
        instance.pids.fetch(pid).actor
      end

      def receiver(pid)
        instance.pids.fetch(pid).receiver
      end
    end
  end
end
