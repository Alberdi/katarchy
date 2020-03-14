-record(mech, {position = undefined :: undefined | {integer(), integer()},
               id = undefined :: undefined | binary(),
               attack_power = 0 :: non_neg_integer(),
               hit_points = 10 :: integer(),
               side = left :: left | right,
               skills = [] :: [atom() | tuple()],
               speed = 0 :: integer()}).
