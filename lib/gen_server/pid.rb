# frozen_string_literal: true

module GenServer
  class PID
    def inspect
      "#<PID:#{format '%#018x', object_id}>"
    end

    alias to_s inspect
  end
end
