use context essentials2021
include reactors 
include shared-gdrive("sprite-lib", "1oXiADjdC5WMA_iljrNdg7Dvo2mJC73_N")
import lists as L
include color

# global constants
FPS = 30
FREQ = 1 / FPS
GRAV-CONSTANT = 100
WIDTH = 1200
HEIGHT = 600
SOFTENER = 80
MASS-NORMALIZATION = 5000
SCALING-CONSTANT = 1
MAX-MASS = 10000
bg = empty-color-scene(WIDTH, HEIGHT, "black")

# data definitions
data Vel: vel(x :: Number, y :: Number) end
data Acc: acc(x :: Number, y :: Number) end
data Entity: 
    entity(pos :: Posn, vel :: Vel, acc :: Acc, mass :: Number) 
end
data Graphic: graphic(ent :: Entity, col :: Color) end
data Path: path(img :: Image, toggled :: Boolean) end
data State: 
  | state(pos :: Posn, graphics :: List<Graphic>, paths :: Path) 
  | intro(title :: String, rules :: List<String>)
end

# FUNCTIONS
fun entity-to-graphic(ent :: Entity) -> Graphic:
  doc: ``` Consumes an entity, produces a graphic of that entity with random color.```
  new-color = color(num-random(255), 
    num-random(255), num-random(255), 1)
  graphic(ent, new-color)
end

fun update-all-pos(ent-list :: List<Entity>) -> List<Entity>:
  doc: ```Consumes a list of entities, updates every position using their respective 
       velocities. ```
  fun update-pos(ent :: Entity) -> Entity:
    new-x = (ent.vel.x * FREQ) + ent.pos.x
    new-y = (ent.vel.y * FREQ) + ent.pos.y
    entity(posn(new-x, new-y), ent.vel, ent.acc, ent.mass)
  end
  map(update-pos, ent-list)
end

fun update-all-vel(ent-list :: List<Entity>) -> List<Entity>:
  doc: ``` Consumes a list of entities, updates the velocity of all entities 
       using their acceleration.```
  fun update-vel(ent :: Entity) -> Entity:
    new-x = (ent.acc.x * FREQ) + ent.vel.x
    new-y = (ent.acc.y * FREQ) + ent.vel.y
    entity(ent.pos, vel(new-x, new-y), ent.acc, ent.mass)
  end
  map(update-vel, ent-list)
end

fun update-all-acc(ent-list :: List<Entity>) -> List<Entity>:
  doc: ``` Consumes a list of entities, updates the acceleration of all entities 
       using the postions and masses of the other entities.```
  fun update-acc(my-ent-list :: List<Entity>) -> List<Entity>:
    cases (List) my-ent-list:
      | empty => empty
      | link(f, r) =>
        acc-list = map(lam(x): calc-pair-acc(f, x) end, ent-list)
        acc-x = foldl(lam(base, elt): base + elt.x end, 0, acc-list)
        acc-y = foldl(lam(base, elt): base + elt.y end, 0, acc-list)
        new-acc = acc(acc-x, acc-y)
        new-entity = entity(f.pos, f.vel, new-acc, f.mass)
        link(new-entity, update-acc(r))
    end
  end
  update-acc(ent-list)
end

fun calc-pair-acc(ent-1 :: Entity, ent-2 :: Entity) -> Acc:
  doc: ```Consumes two entities, calculates the acceleration due to gravity between 
       them on the first entity.```
  if identical(ent-1, ent-2):
    acc(0, 0)
  else:
    delta-x = (ent-1.pos.x - ent-2.pos.x) * ~1
    delta-y = (ent-1.pos.y - ent-2.pos.y) * ~1
    r-squared = num-sqr(delta-x) + num-sqr(delta-y) + num-sqr(SOFTENER)
    mass = ent-2.mass
    acc-mag = (GRAV-CONSTANT * mass) / (r-squared)
    acc-x = ((delta-x / num-sqrt(r-squared)) * -1 * acc-mag)
    acc-y = ((delta-y / num-sqrt(r-squared)) * -1 * acc-mag)
    acc(acc-x, acc-y)
  end
end

fun random-graphics-list(max-vel :: Number, max-acc :: Number, 
    max-mass :: Number,num-entities :: Number) -> List<Graphic>:
  doc: ``` Consumes maximum value parameters and a number k, generates k 
       graphics with parameters bounded by the given maximum values.```
  if num-entities == 0:
    empty
  else:
    new-entity = entity(posn(num-random(WIDTH), num-random(HEIGHT)), 
      vel(num-random(max-vel), num-random(max-vel)), 
      acc(num-random(max-acc), num-random(max-acc)), 
      num-random(max-mass))
    new-graphic = entity-to-graphic(new-entity)
    link(new-graphic, 
      random-graphics-list(max-vel, max-acc, max-mass, num-entities - 1))
  end
end

fun update-paths(graphics:: List<Graphic>, old-paths :: Image) -> Image:
  doc: ```Consumes a list of graphics, adds the position of each 
       graphic's entity to the state's path image.```
  cases(List) graphics:
    | empty => old-paths
    | link(f, r) =>
      ent = f.ent
      place-image(circle(
          (1), "solid", f.col), 
        ent.pos.x, ent.pos.y, update-paths(r, old-paths))
  end
end

fun update-graphics(graphics :: List<Graphic>) -> List<Graphic>:
  doc: "Consumes a list of graphics and updates all values in the given list."
  entities = map(lam(val): val.ent end, graphics)
  new-pos = update-all-pos(entities)
  new-vel = update-all-vel(entities)
  new-acc = update-all-acc(entities)

  make-new-entity = lam(pos-ent, vel-ent, acc-ent): 
  entity(pos-ent.pos, vel-ent.vel, acc-ent.acc, pos-ent.mass) end
  make-new-graphic = lam(ent, my-graphic): 
  graphic(ent, my-graphic.col) end
  new-entities = map3(make-new-entity, new-pos, new-vel, new-acc)
  map2(make-new-graphic, new-entities, graphics)
end

fun update-state(current-state :: State) -> State:
  doc: ``` Given a list of graphics, updates all of their positions,
       velocities and accelerations.```
  cases(State) current-state:
    | intro(title, rules) => current-state
    | state(pos, graphics, paths) =>
      new-graphics = update-graphics(graphics)
      new-paths = 
        if paths.toggled:
          path(update-paths(new-graphics, paths.img), paths.toggled)
        else: paths
        end
      state(pos, new-graphics, new-paths)
  end
end

fun show(current-state :: State) -> Image:
  doc: "Given a state, displays the image within the current state."
  cases(State) current-state:
    | intro(title, rules) => 
      title-image = text(title, 40, "black")
      rules-images = map(lam(string): text(string, 24, "black") end, rules)
      len = rules-images.length()
      fun add-rules(my-rules :: List<String>, height :: Number) -> Image:
        cases(List) my-rules:
          | empty => empty-scene(WIDTH, HEIGHT)
          | link(f, r) => 
            place-image(f, WIDTH / 2, (HEIGHT / 2.5) + height, 
              add-rules(r, height + (HEIGHT / (len * 2))))
        end
      end
      place-image(title-image, WIDTH / 2, HEIGHT / 3.5, 
        add-rules(rules-images, 0))
    | state(pos, graphics, paths) =>
      cases (List) graphics:
        | empty => 
          if paths.toggled: paths.img else: bg end
        | link(f, r) => 
          ent = f.ent
          rest-graphics = state(pos, r, paths)
          place-image(
            circle((15 * (ent.mass / MASS-NORMALIZATION)), "solid", f.col), 
            ent.pos.x, ent.pos.y, show(rest-graphics))
      end
  end  
end

fun add-graphic(current-state :: State, x :: Number, y :: Number, 
    action :: String) -> State:
  doc: ```Given a current state, coordinates, and mouse action, allows 
       the user to click and drag to produce a new graphic of random mass, 
       no acceleration, position at initial click, and velocity based on 
       direction and magnitude of the relative final mouse position.```
  cases(State) current-state:
    | intro(title, rules) => current-state
    | state(pos, graphics, paths) =>
      ask:
        | action == "button-down" then: 
          state(posn(x, y), graphics, paths)
        | action == "button-up" then:
          vel-x = (x - pos.x) * SCALING-CONSTANT
          vel-y = (y - pos.y) * SCALING-CONSTANT
          new-vel = vel(vel-x, vel-y)
          new-mass = num-random(MAX-MASS)
          new-entity = entity(pos, new-vel, acc(0, 0), new-mass)
          new-graphic = entity-to-graphic(new-entity)
          new-graphics-list = link(new-graphic, graphics)
          state(posn(0, 0), new-graphics-list, paths)
        | otherwise: current-state
      end
  end
end

# reactor presets
EQ-SL = 300
EQ-V = 40
EQ-M = 8000
EQ-A = acc(0, 0)
equilateral-triangle = [list: 
  graphic(entity(
      posn(WIDTH / 2,  (HEIGHT / 2) - ((EQ-SL * num-sqrt(3)) / 4)), 
      vel(EQ-V, 0), EQ-A, EQ-M), color-named("red")), 
  graphic(entity(
      posn((WIDTH / 2) + (EQ-SL / 2), 
        (HEIGHT / 2) + ((EQ-SL * num-sqrt(3)) / 4)), 
      vel(-1 * (EQ-V * num-cos(PI / 3)), (EQ-V * num-sin(PI / 3))), 
      EQ-A, EQ-M), color-named("blue")), 
  graphic(entity(
      posn((WIDTH / 2) - (EQ-SL / 2), 
        (HEIGHT / 2) + ((EQ-SL * num-sqrt(3)) / 4)), 
      vel(-1 * (EQ-V * num-cos(PI / 3)), -1 * (EQ-V * num-sin(PI / 3))),
      EQ-A, EQ-M), color-named("yellow"))]
five-random-pos = random-graphics-list(0, 0, MAX-MASS, 5)
ten-random-pos = random-graphics-list(0, 0, MAX-MASS, 10)

fun start-sim(current-state :: State, key :: String) -> State:
  doc: ``` Given a state and key input, starts the simulation based on the type
       of key input. Additionally handles resets and path toggling.```
  cases(State) current-state:
    | intro(title, rules) =>
      ask:
        | key == "enter" then: state(posn(0, 0), empty, path(bg, true))
        | key == "t" then: state(posn(0, 0), 
            equilateral-triangle, path(bg, true))
        | key == "f" then: state(posn(0, 0), five-random-pos, path(bg, true))
        | key == "p" then: state(posn(0, 0), ten-random-pos, path(bg, true))
        | otherwise: current-state
      end
    | state(pos, graphics, paths) =>
      ask:
        | key == "enter" then: state(posn(0, 0), empty, 
            path(bg, paths.toggled))
        | key == "r" then: state(pos, graphics, 
            path(bg, not(paths.toggled)))
        | otherwise: current-state
      end
  end
end

# starting reactor
welcome = "Welcome to the N-Body Simulator!"
explanation = ```This program simulates the effects of gravity on point masses in space.```
inter-part-1 = ```To add point masses, click where you want to add a mass and drag ```
inter-part-2 = ```in the direction you want it to move.```
interaction = inter-part-1 + inter-part-2
interaction-2 = "The futher you drag, the faster it moves." 
reset = "Once you have started, press enter to reset the simulation and r to toggle paths"
start = "Select your desired initial condition. Enjoy!"
initial-conditions = "Empty: enter; Triangle: t; Five randoms: f; Ten randoms: p"

title = "N-Body Simulator"
rules = [list: welcome, explanation, interaction, interaction-2, 
  reset, start, initial-conditions]

n-body-simulator = reactor:
  seconds-per-tick: FREQ,
  init: intro(title, rules),
  on-tick: update-state, 
  on-mouse: add-graphic,
  to-draw: show,
  on-key: start-sim
end

interact(n-body-simulator)

# TESTING SPACE
# variables for testing
num-1 = num-random(2150)
num-2 = num-random(91207591875)
num-3 = num-random(12)
num-4 = num-random(5128975)

ent-empty = entity(posn(0, 0), vel(0, 0), acc(0, 0), 0)
ent-pos = entity(posn(5, 10), vel(0, 0), acc(0, 0), 0)
ent-vel = entity(posn(0, 0), vel(3, 4), acc(0, 0), 0)
ent-acc = entity(posn(0, 0), vel(0, 0), acc(3, 4), 0)
ent-mass = entity(posn(0, 0), vel(0, 0), acc(0, 0), 50)
ent-all = entity(posn(5, 10), vel(2, 4), acc(3, 6), 50)
ent-negatives = entity(posn(-5, -10), vel(-2, -4), acc(-3, -6), 0)
ent-irrational = entity(posn(num-sqrt(5), -10), vel(-2, -4), acc(-3, -6), num-sqrt(73))
ent-rough-nums = entity(posn(~-5.2, ~-10.4), vel(~-2.6, ~-4.3), acc(~-3.1, ~-6.8), ~10.2)
ent-negatives-with-mass = entity(posn(-5, -10), vel(-2, -4), acc(-3, -6), 100)
ent-high-mass = entity(posn(-20, -30), vel(0, 0), acc(0, 0), 10000)
ent-low-mass = entity(posn(5, 10), vel(0, 0), acc(0, 0), 10)

ent-list-1 = [list: ent-pos, ent-vel, ent-acc, ent-mass]
ent-list-2 = [list: ent-empty, ent-all]
ent-list-3 = [list: ent-negatives, ent-rough-nums]
ent-list-4 = [list: ent-negatives-with-mass, ent-rough-nums]
ent-list-5 = [list: ent-low-mass, ent-mass, ent-high-mass]

graphic-empty = graphic(entity(posn(100, 100), vel(0, 0), acc(0, 0), 0), 
  color(10, 10, 10, 1))
graphic-empty-moved = graphic(entity(posn(101, 101), vel(0, 0), acc(0, 0), 0), 
  color(10, 10, 10, 1))
graphic-pos = graphic(entity(posn(105, 110), vel(0, 0), acc(0, 0), 0), 
  color(20, 20, 20, 1))
graphic-pos-moved = graphic(entity(posn(106, 111), vel(0, 0), acc(0, 0), 0), 
  color(20, 20, 20, 1))
graphic-vel = graphic(entity(posn(0, 0), vel(3, 4), acc(0, 0), 0), 
  color(30, 30, 30, 1))
graphic-acc = graphic(entity(posn(0, 0), vel(0, 0), acc(3, 4), 0), 
  color(40, 40, 40, 1))
graphic-mass = graphic(entity(posn(0, 0), vel(0, 0), acc(0, 0), 50), 
  color(50, 50, 50, 1))
graphic-all = graphic(entity(posn(8, 12), vel(2, 4), acc(3, 6), 50), 
  color(60, 60, 60, 1))

equilateral-sim = react(n-body-simulator, keypress("t"))
empty-sim = react(n-body-simulator, keypress("enter"))
empty-sim-2 = react(empty-sim, mouse(num-1, num-2, "button-down"))

intro-state = intro("random", [list: "things"])
empty-state = state(posn(0, 0), empty, path(empty-scene(10, 10), true))
simple-state-graphics = [list: graphic-empty, graphic-all, graphic-mass]
simple-state = state(posn(0, 0), simple-state-graphics, 
  path(empty-scene(10, 10), true))
complex-state-graphics = [list: graphic-empty, graphic-all, graphic-mass, 
  graphic-mass, graphic-pos, graphic-acc]
complex-state = state(posn(10, 10), complex-state-graphics, 
  path(empty-scene(10, 10), false))

# testing blocks
check "Checking to make sure the initial reactor states are reasonable":
  # initial state
  get-value(n-body-simulator) is intro(title, rules)
  # starting states
  get-value(react(n-body-simulator, keypress("enter"))) is 
  state(posn(0, 0), [list: ], path(bg, true))
  get-value(react(n-body-simulator, keypress("f"))) is 
  state(posn(0, 0), five-random-pos, path(bg, true))
  get-value(react(n-body-simulator, keypress("p"))) is 
  state(posn(0, 0), ten-random-pos, path(bg, true))
  get-value(react(n-body-simulator, keypress("t"))) is 
  state(posn(0, 0), equilateral-triangle, path(bg, true))
end

check "Checking to make sure the in-simulation reactor states are reasonable":
  # checking toggle paths
  get-value(react(empty-sim, keypress("r"))) is 
  state(posn(0, 0), empty, path(bg, false))
  get-value(react(react(empty-sim, keypress("r")), keypress("r"))) is 
  state(posn(0, 0), empty, path(bg, true))
  # checking empty simulation states
  get-value(empty-sim-2) is 
  state(posn(num-1, num-2), empty, path(bg, true))
  get-value(react(empty-sim-2, mouse(num-1, num-2, "button-up"))).graphics.get(0).ent.pos is
  posn(num-1, num-2)
  get-value(react(empty-sim-2, mouse(num-1, num-2, "button-up"))).graphics.get(0).ent.vel is
  vel(0, 0)
  get-value(react(empty-sim-2, mouse(num-1, num-2, "button-up"))).graphics.get(0).ent.acc is
  acc(0, 0)
  get-value(react(empty-sim, keypress("enter"))) is
  state(posn(0, 0), empty, path(bg, true))
  get-value(react(empty-sim-2, keypress("enter"))) is
  state(posn(0, 0), empty, path(bg, true))
  # checking equilateral simulation states
  get-value(equilateral-sim) is 
  state(posn(0, 0), equilateral-triangle, path(bg, true))
  get-value(react(equilateral-sim, time-tick)) is-roughly
  update-state(state(posn(0, 0), equilateral-triangle, path(bg, true)))
  get-value(react(equilateral-sim, keypress("r"))) is
  state(posn(0, 0), equilateral-triangle, path(bg, false))
  get-value(react(equilateral-sim, keypress("enter"))) is
  state(posn(0, 0), empty, path(bg, true))
  get-value(react(react(equilateral-sim, mouse(num-1, num-2, "button-down")), 
      mouse(num-1 + 5, num-2 + 5, "button-up"))).graphics.length() is 4
  get-value(react(react(equilateral-sim, mouse(num-1, num-2, "button-down")), 
      mouse(num-1 + 5, num-2 - 21598.2, "button-up"))).graphics.length() is 4
end

check "Checking 'entity-to-graphic' for non-rough parameters":
  # property based testing as the associated color is random.
  (entity-to-graphic(entity(posn(0, 0), vel(0, 0), acc(0, 0), 0))).ent is
  entity(posn(0, 0), vel(0, 0), acc(0, 0), 0)
  (entity-to-graphic(entity(posn(0, 5), vel(-2, 65), acc(3, 0), 10 * 34))).ent is
  entity(posn(0, 5), vel(-2, 65), acc(3, 0), 10 * 34)
end
check "Checking 'entity-to-graphic' for rough-num-requiring parameters":
  # non-integer rationals
  (entity-to-graphic(entity(posn(WIDTH / 2,  (HEIGHT / 2) - ((5 * num-sqrt(3)) / 4)), 
        vel(5, 0), acc(5, 5), 5))).ent is-roughly
  entity(posn(WIDTH / 2,  (HEIGHT / 2) - ((5 * num-sqrt(3)) / 4)), 
    vel(5, 0), acc(5, 5), 5)
  (entity-to-graphic(entity(posn(12959125, 3128512.5215312), 
        vel(-2195, 37125.219), acc(0.0000024, 0), 0))).ent is-roughly
  entity(posn(12959125, 3128512.5215312), 
    vel(-2195, 37125.219), acc(0.0000024, 0), 0)
  # irrationals
  (entity-to-graphic(entity(posn(num-sqrt(7.2), -1 * 3128512.5215312), 
        vel(-2195, 37125.219), acc(-0.0000024, num-sqrt(2)), 0))).ent is-roughly
  entity(posn(num-sqrt(7.2), -3128512.5215312), 
    vel(-2195, 37125.219), acc(-0.0000024, num-sqrt(2)), 0)
end

check "Checking 'update-all-pos' for non-rough parameters":
  update-all-pos(empty) is empty
  update-all-pos([list: ent-empty]) is
  [list: entity(posn(0, 0), vel(0, 0), acc(0, 0), 0)]
  update-all-pos(ent-list-1) is 
  [list: 
    entity(posn(5, 10), vel(0, 0), acc(0, 0), 0), 
    entity(posn(3 * FREQ, 4 * FREQ), vel(3, 4), acc(0, 0), 0), 
    entity(posn(0, 0), vel(0, 0), acc(3, 4), 0), 
    entity(posn(0, 0), vel(0, 0), acc(0, 0), 50)]
  update-all-pos(ent-list-2) is 
  [list: 
    entity(posn(0, 0), vel(0, 0), acc(0, 0), 0), 
    entity(posn(5 + (2 * FREQ), 10 + (4 * FREQ)), 
      vel(2, 4), acc(3, 6), 50)]
end
check "Checking 'update-all-pos' for rough-num-requiring parameters":
  # non-integer rationals
  update-all-pos(ent-list-3) is-roughly
  [list: 
    entity(posn(-5 - (2 * FREQ), -10 - (4 * FREQ)), 
      vel(-2, -4), acc(-3, -6), 0), 
    entity(posn(-5.2 - (2.6 * FREQ), -10.4 - (4.3 * FREQ)), 
      vel(-2.6, -4.3), acc(-3.1, -6.8), 10.2)]
  # irrationals
  update-all-pos([list: ent-irrational]) is-roughly
  [list: entity(posn(num-sqrt(5) + (-2 * FREQ), -10 + (-4 * FREQ)), 
      vel(-2, -4), acc(-3, -6), num-sqrt(73))]
end

check "Checking 'update-all-vel' for non-rough parameters":
  update-all-vel(empty) is empty
  update-all-vel([list: ent-empty]) is
  [list: entity(posn(0, 0), vel(0, 0), acc(0, 0), 0)]
  update-all-vel(ent-list-1) is 
  [list: 
    entity(posn(5, 10), vel(0, 0), acc(0, 0), 0), 
    entity(posn(0, 0), vel(3, 4), acc(0, 0), 0), 
    entity(posn(0, 0), vel(3 * FREQ, 4 * FREQ), acc(3, 4), 0), 
    entity(posn(0, 0), vel(0, 0), acc(0, 0), 50)]
  update-all-vel(ent-list-2) is 
  [list: 
    entity(posn(0, 0), vel(0, 0), acc(0, 0), 0), 
    entity(posn(5, 10), vel(2 + (3 * FREQ), 4 + (6 * FREQ)), 
      acc(3, 6), 50)]
end
check "Checking 'update-all-vel' for rough-num-requiring parameters":
  # non-integer rationals
  update-all-vel(ent-list-3) is-roughly
  [list: 
    entity(posn(-5, -10), 
      vel(-2 - (3 * FREQ), -4 - (6 * FREQ)), acc(-3, -6), 0), 
    entity(posn(-5.2, -10.4), 
      vel(-2.6 - (3.1 * FREQ), -4.3 - (6.8 * FREQ)), 
      acc(-3.1, -6.8), 10.2)]
  # irrationals
  update-all-vel([list: ent-irrational]) is-roughly
  [list: entity(posn(num-sqrt(5), -10), 
      vel(-2 + (-3 * FREQ), -4 + (-6 * FREQ)), acc(-3, -6), num-sqrt(73))]
  update-all-vel(link(ent-irrational, ent-list-1)) is-roughly
  [list: entity(posn(num-sqrt(5), -10), 
      vel(-2 + (-3 * FREQ), -4 + (-6 * FREQ)), acc(-3, -6), num-sqrt(73)), 
    entity(posn(5, 10), vel(0, 0), acc(0, 0), 0), 
    entity(posn(0, 0), vel(3, 4), acc(0, 0), 0), 
    entity(posn(0, 0), vel(3 * FREQ, 4 * FREQ), acc(3, 4), 0), 
    entity(posn(0, 0), vel(0, 0), acc(0, 0), 50)]
end

check "Checking that 'random-graphics-list' creates a list of graphics of correct length.":
  random-graphics-list(0, -1 * num-sqrt(num-2), num-3, 0).length() is 0
  random-graphics-list(num-1, num-2, num-3, 0).length() is 0
  random-graphics-list(-1 * 125512, num-sqrt(num-4), num-4, num-3).length() is num-3
  random-graphics-list(num-random(2150), num-random(2150), 
    num-random(2150), num-1).length() is num-1
  random-graphics-list(num-1, num-3, num-4, num-1).length() is num-1
  random-graphics-list((-72 * num-sqrt(num-2)) * num-2, num-2, num-2, 
    num-3 * num-1).length() is num-3 * num-1
end

check "Checking 'start-sim' for introductory states.":
  start-sim(intro-state, "random") is intro-state
  start-sim(intro-state, "enter") is state(posn(0, 0), empty, path(bg, true))
  start-sim(intro-state, "t") is state(posn(0, 0), 
    equilateral-triangle, path(bg, true))
  start-sim(intro-state, "f") is state(posn(0, 0), 
    five-random-pos, path(bg, true))
  start-sim(intro-state, "p") is state(posn(0, 0), ten-random-pos, 
    path(bg, true))
end
check "Checking 'start-sim' for in-simulation states.":
  start-sim(simple-state, "anything") is simple-state
  start-sim(simple-state, "r") is state(posn(0, 0), simple-state-graphics,
    path(bg, false))
  start-sim(complex-state, "r") is state(posn(10, 10), complex-state-graphics, 
    path(bg, true))
  start-sim(simple-state, "enter") is state(posn(0, 0), empty, 
    path(bg, true))
  start-sim(complex-state, "enter") is state(posn(0, 0), empty, 
    path(bg, false))
end

check "Checking 'add-graphic' for intro situations.":
  add-graphic(intro-state, 0, 0, "button-down") is intro-state
  add-graphic(intro-state, 0, 0, "button-up") is intro-state
  add-graphic(intro-state, 20, 25, "random") is intro-state
  add-graphic(intro-state, num-sqrt(num-random(21581)), -420, "button-up") is intro-state
end
check "Checking 'add-graphic' for in-simulation situations.":
  add-graphic(empty-state, 10, 20, "random") is empty-state
  state-after-up = add-graphic(add-graphic(empty-state, 10, 20, "button-down"),
    30, 40, "button-up")
  state-after-up.pos is posn(0, 0)
  graphic-list-after-up = state-after-up.graphics
  graphic-list-after-up.length() is 1
  graphic-after-up = graphic-list-after-up.get(0)
  ent-after-up = graphic-after-up.ent
  ent-after-up.pos is posn(10, 20)
  ent-after-up.vel is vel(20, 20)
  ent-after-up.acc is acc(0, 0)
  state-after-up.paths is path(empty-scene(10, 10), true)
  add-graphic(empty-state, 10, 20, "button-down") is 
  state(posn(10, 20), empty, path(empty-scene(10, 10), true))
end

check "Checking 'calc-pair-accel' for 0 cases.":
  #zeros
  calc-pair-acc(ent-empty, ent-empty) is acc(0, 0)
  calc-pair-acc(ent-all, ent-all) is acc(0, 0)
  calc-pair-acc(ent-all, ent-empty) is acc(0, 0)
end
check "Checking 'calc-pair-accel' for various inputs (ints, irrationals, etc.)":
  #positive, regular nums
  calc-pair-acc(ent-mass, ent-all) is-roughly
  acc((GRAV-CONSTANT * 50 * 5) / num-expt((num-sqrt(125 + num-sqr(SOFTENER))), 3), 
    (GRAV-CONSTANT * 50 * 10) / num-expt((num-sqrt(125 + num-sqr(SOFTENER))), 3))
  calc-pair-acc(ent-all, ent-mass) is-roughly
  acc((GRAV-CONSTANT * 50 * -5) / num-expt((num-sqrt(125 + num-sqr(SOFTENER))), 3), 
    (GRAV-CONSTANT * 50 * -10) / num-expt((num-sqrt(125 + num-sqr(SOFTENER))), 3))
  #big diff in masses
  calc-pair-acc(ent-high-mass, ent-low-mass) is-roughly
  acc((GRAV-CONSTANT * 10 * 25) / num-expt(num-sqrt(2225 + num-sqr(SOFTENER)), 3), 
    (GRAV-CONSTANT * 10 * 40) / num-expt(num-sqrt(2225 + num-sqr(SOFTENER)), 3))
  calc-pair-acc(ent-low-mass, ent-high-mass) is-roughly
  acc((GRAV-CONSTANT * 10000 * -25) / num-expt(num-sqrt(2225 + num-sqr(SOFTENER)), 3), 
    (GRAV-CONSTANT * 10000 * -40) / num-expt(num-sqrt(2225 + num-sqr(SOFTENER)), 3))
  #negative and rough nums
  calc-pair-acc(ent-rough-nums, ent-negatives-with-mass) is-roughly
  acc((GRAV-CONSTANT * 100 * 0.2) / num-expt(num-sqrt(0.2 + num-sqr(SOFTENER)), 3), 
    (GRAV-CONSTANT * 100 * 0.4) / num-expt(num-sqrt(0.2 + num-sqr(SOFTENER)), 3))
  calc-pair-acc(ent-negatives-with-mass, ent-rough-nums) is-roughly
  acc((GRAV-CONSTANT * 10.2 * -0.2) / num-expt(num-sqrt(0.2 + num-sqr(SOFTENER)), 3), 
    (GRAV-CONSTANT * 10.2 * -0.4) / num-expt(num-sqrt(0.2 + num-sqr(SOFTENER)), 3))
end

check "Checking 'update-all-accel' for various inputs (ints, irrationals, etc.)":
  update-all-acc(empty) is empty
  update-all-acc([list: ent-all]) is [list: 
    entity(ent-all.pos, ent-all.vel, acc(0, 0), ent-all.mass)]
  update-all-acc([list: ent-all, ent-all]) is [list: 
    entity(ent-all.pos, ent-all.vel, acc(0, 0), ent-all.mass),
    entity(ent-all.pos, ent-all.vel, acc(0, 0), ent-all.mass)]

  update-all-acc(ent-list-4) is-roughly [list: 
    entity(posn(-5, -10), vel(-2, -4), 
      calc-pair-acc(ent-negatives-with-mass, ent-rough-nums), 100), 
    entity(posn(~-5.2, ~-10.4), vel(~-2.6, ~-4.3), 
      calc-pair-acc(ent-rough-nums, ent-negatives-with-mass), ~10.2)]

  update-all-acc(ent-list-5) is-roughly [list:
    entity(posn(5, 10), vel(0, 0), 
      acc(calc-pair-acc(ent-low-mass, ent-mass).x + 
        calc-pair-acc(ent-low-mass, ent-high-mass).x, 
        calc-pair-acc(ent-low-mass, ent-mass).y + 
        calc-pair-acc(ent-low-mass, ent-high-mass).y), 10), 
    entity(posn(0, 0), vel(0, 0),
      acc(calc-pair-acc(ent-mass, ent-low-mass).x + 
        calc-pair-acc(ent-mass, ent-high-mass).x, 
        calc-pair-acc(ent-mass, ent-low-mass).y + 
        calc-pair-acc(ent-mass, ent-high-mass).y), 50), 
    entity(posn(-20, -30), vel(0, 0), 
      acc(calc-pair-acc(ent-high-mass, ent-mass).x + 
        calc-pair-acc(ent-high-mass, ent-low-mass).x, 
        calc-pair-acc(ent-high-mass, ent-mass).y + 
        calc-pair-acc(ent-high-mass, ent-low-mass).y), 10000)]
end

check "Checking 'update-state' for various states.":
  update-state(intro-state) is intro-state
  update-state(empty-state) is empty-state 
  update-state(simple-state) is-roughly state(posn(0, 0), 
    update-graphics(simple-state-graphics), 
    path(update-paths(simple-state-graphics, 
        empty-scene(10, 10)), true))
  update-state(complex-state) is-roughly state(posn(10, 10), 
    update-graphics(complex-state-graphics), 
    path(empty-scene(10, 10), false))
end

check "Checking 'update-paths' for various graphics.":
  update-paths(empty, bg) is bg
  update-paths([list: graphic-empty], bg) is 
  place-image(circle((1), "solid", color(10, 10, 10, 1)), 100, 100, bg)
  update-paths([list: graphic-empty, graphic-pos], bg) is 
  place-image(circle((1), "solid", color(20, 20, 20, 1)), 105, 110, 
    update-paths([list: graphic-empty], bg))
  update-paths([list: graphic-empty, graphic-pos, graphic-all], bg) is 
  place-image(circle((1), "solid", color(60, 60, 60, 1)), 8, 12, 
    update-paths([list: graphic-empty, graphic-pos], bg))
end

check "Checking 'update-graphics' for various graphics.":
  update-graphics(empty) is empty
  update-graphics([list: graphic-empty]) is [list: graphic-empty]
  graphic-1 = graphic(entity(posn(0, 0), vel(3, 4), acc(3, 4), 10), 
    color(30, 30, 30, 1))
  graphic-2 = graphic(entity(posn(10, 10), vel(3, 4), acc(3, 4), 20), 
    color(40, 40, 40, 1))
  update-graphics([list: graphic-1]) is 
  [list: graphic(entity(posn(3 * FREQ, 4 * FREQ), 
        vel(3 + (3 * FREQ), 4 + (4 * FREQ)),
        acc(0, 0), 10), color(30, 30, 30, 1))]
  update-graphics([list: graphic-1, graphic-2]) is-roughly 
  [list: graphic(entity(posn(3 * FREQ, 4 * FREQ), 
        vel(3 + (3 * FREQ), 4 + (4 * FREQ)),
        calc-pair-acc(graphic-1.ent, graphic-2.ent), 10), color(30, 30, 30, 1)), 
    graphic(entity(posn(10 + (3 * FREQ), 10 + (4 * FREQ)), 
        vel(3 + (3 * FREQ), 4 + (4 * FREQ)),
        calc-pair-acc(graphic-2.ent, graphic-1.ent), 20), color(40, 40, 40, 1))]
end

check "Checking 'show' for various states.":
  show(state(posn(0, 0), empty, path(bg, true))) is bg
  show(state(posn(0, 0), [list: graphic(entity(
            posn(WIDTH / 2,  (HEIGHT / 2) - ((EQ-SL * num-sqrt(3)) / 4)), 
            vel(EQ-V, 0), EQ-A, EQ-M), color-named("red"))], path(bg, true))) is
  place-image(circle((15 * (EQ-M / MASS-NORMALIZATION)), "solid", color-named("red")), 
    WIDTH / 2, (HEIGHT / 2) - ((EQ-SL * num-sqrt(3)) / 4), bg)
  show(state(posn(0, 0), [list: graphic-empty, graphic-pos], 
      path(empty-scene(num-1, num-3), true))) is 
  place-image(circle((1), "solid", color(10, 10, 10, 1)), 100, 100, 
    place-image(circle((1), "solid", color(20, 20, 20, 1)), 105, 110, empty-scene(num-1, num-3)))
end