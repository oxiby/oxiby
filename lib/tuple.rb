module Std
  module Tuple
    class Tuple
      attr_accessor :fields

      def initialize(fields:)
        self.fields = fields
      end

      def deconstruct
        fields
      end

      def method_missing(index, *args)
        name = index.to_s[2..]

        self.class.define_method(name) { fields[name.to_i] }

        public_send(name)
      end
    end
  end
end
