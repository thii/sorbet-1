# frozen_string_literal: true
# typed: true

module T::Types
  # Modeling self-types properly at runtime would require additional tracking,
  # so at runtime we permit all values and rely on the static checker.
  class SelfType < Base

    def initialize(); end

    # @override Base
    def name
      "T.self_type"
    end

    # @override Base
    def valid?(obj)
      true
    end

    # @override Base
    private def subtype_of_single?(other)
      case other
      when SelfType
        true
      else
        false
      end
    end

    module Private
      INSTANCE = SelfType.new.freeze
    end
  end
end
