# frozen_string_literal: true

require_relative "std/all"

module TicTacToe
  class Player
    class X
      def to_s
        "X"
      end

      def ==(other)
        self.class == other.class
      end
    end

    class O
      def to_s
        "O"
      end

      def ==(other)
        self.class == other.class
      end
    end
  end

  class Winner
    class Player
      attr_accessor :__0

      def initialize(__0)
        self.__0 = __0
      end

      def to_s
        "Player"
      end

      def ==(other)
        __0 == other.__0
      end

      def deconstruct
        [__0]
      end
    end

    class Draw
      def to_s
        "Draw"
      end

      def ==(other)
        self.class == other.class
      end
    end

    class NoneYet
      def to_s
        "NoneYet"
      end

      def ==(other)
        self.class == other.class
      end
    end
  end

  class Game
    attr_accessor :player, :rows

    def initialize(player:, rows:)
      self.player = player
      self.rows = rows
    end

    def to_s
      "Game { player: #{player}, rows: #{rows} }"
    end

    def ==(other)
      player == other.player && rows == other.rows
    end
    def deconstruct_keys(_keys)
      { player:, rows: }
    end

    def winner
      ((0..2)).each do |row|
        case ::Std::Tuple::Tuple.new(fields: [rows[row][0], rows[row][1], rows[row][2]])
        in [::Std::Option::Option::Some(a), ::Std::Option::Option::Some(b), ::Std::Option::Option::Some(c)]
            if a == b && b == c
              return Winner::Player.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, a) }
            end

        else
          ::Std::Tuple::Tuple.new(fields: [])
        end
      end
      ((0..2)).each do |column|
        case ::Std::Tuple::Tuple.new(fields: [rows[0][column], rows[1][column], rows[2][column]])
        in [::Std::Option::Option::Some(a), ::Std::Option::Option::Some(b), ::Std::Option::Option::Some(c)]
            if a == b && b == c
              return Winner::Player.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, a) }
            end

        else
          ::Std::Tuple::Tuple.new(fields: [])
        end
      end
      case ::Std::Tuple::Tuple.new(fields: [rows[0][0], rows[1][1], rows[2][2]])
      in [::Std::Option::Option::Some(a), ::Std::Option::Option::Some(b), ::Std::Option::Option::Some(c)]
          if a == b && b == c
            return Winner::Player.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, a) }
          end

      else
        ::Std::Tuple::Tuple.new(fields: [])
      end
      case ::Std::Tuple::Tuple.new(fields: [rows[0][2], rows[1][1], rows[2][0]])
      in [::Std::Option::Option::Some(a), ::Std::Option::Option::Some(b), ::Std::Option::Option::Some(c)]
          if a == b && b == c
            return Winner::Player.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, a) }
          end

      else
        ::Std::Tuple::Tuple.new(fields: [])
      end
      (rows).each do |row|
        (row).each do |column|
          case column
          in ::Std::Option::Option::Some(player)
            ::Std::Tuple::Tuple.new(fields: [])
          in ::Std::Option::Option::None
            return Winner::NoneYet.allocate
          end
        end
      end
      Winner::Draw.allocate
    end

    def choose(cell, player)
      if !(1..9).contains(cell)
        return false
      end
      case rows[(cell - 1) / 3][(cell - 1) % 3]
      in ::Std::Option::Option::Some
        false
      in ::Std::Option::Option::None
          rows[(cell - 1) / 3][(cell - 1) % 3] = ::Std::Option::Option::Some.new(player)
          true

      end
    end
  end

  def self.main
    game = Game.allocate.tap { |__oxiby_new| __oxiby_new.send(:initialize, player: Player::X.allocate, rows: ::Std::List::List.times(3, ->  { ::Std::List::List.times(3, ->  { ::Std::Option::Option::None.new }) })) }
    loop do
      ::Std::Io.print_line("Game board:")
      (game.rows).each do |row|
        (row).each do |cell|
          case cell
          in ::Std::Option::Option::Some(player)
            ::Std::Io.print(player)
          in ::Std::Option::Option::None
            ::Std::Io.print(".")
          end
        end
        ::Std::Io.print_line("")
      end
      ::Std::Io.print_line("")
      case game.winner
      in Winner::Player(player)
          ::Std::Io.print_line("The winner is #{player}!")
          break

      in Winner::Draw
          ::Std::Io.print_line("It's a draw!")
          break

      in Winner::NoneYet
        ::Std::Tuple::Tuple.new(fields: [])
      end
      loop do
        ::Std::Io.print("It's #{game.player}'s turn. Enter the cell to play (1-9): ")
        case ::Std::Io.read_line.map(-> (s) { s.to_i })
        in ::Std::Result::Result::Ok(cell)
            if game.choose(cell, game.player)
              game.player = case game.player
              in Player::X
                Player::O.allocate
              in Player::O
                Player::X.allocate
              end
              break
            end

        else
          ::Std::Tuple::Tuple.new(fields: [])
        end
        ::Std::Io.print_line("")
        ::Std::Io.print_line("You entered an invalid cell number.")
      end
      ::Std::Io.print_line("")
    end
  end

  main
end