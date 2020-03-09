-record(mech, {position = undefined :: undefined | {integer(), integer()},
               attack_power = 0 :: pos_integer(),
               hit_points = 10 :: integer(),
               side = left :: left | right,
               skills = [] :: [atom()],
               speed = 0 :: integer()}).
