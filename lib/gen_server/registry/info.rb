# frozen_string_literal: true

module GenServer
  class Registry
    Info = Struct.new(:actor, :receiver, keyword_init: true)
  end
end
