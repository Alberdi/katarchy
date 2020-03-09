-record(mech, {position = undefined :: undefined | {integer(), integer()},
               hit_points = 10 :: integer(),
               side = left :: left | right,
               skills = [] :: [atom()],
               speed = 0 :: integer()}).
