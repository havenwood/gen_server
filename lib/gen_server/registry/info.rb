# frozen_string_literal: true

module GenServer
  class Registry
    Info = Struct.new(:actor, :klass, keyword_init: true)
  end
end
