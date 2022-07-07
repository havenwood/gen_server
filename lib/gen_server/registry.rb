# frozen_string_literal: true

require 'singleton'
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

      def [](pid)
        instance.pids[pid]
      end

      def fetch(pid)
        instance.pids.fetch(pid)
      end

      def delete(pid)
        instance.pids.delete(pid)
      end
    end
  end
end
