# SC2 Analyzer

So this idea came up when I was waking up this morning and thought about the
entire game of starcraft could be thought of as a list of actions, and the
times those actions were executed. From there, you could also feed in
information such as build times of certain buildings and units and so forth,
and from there you would have a way to represent a build order as a function in
functional programming.

So to start to make this idea a little more concrete, let's start defining
some things. First, lets define our enumerations.

data Unit
    = Probe
    | Zealot
    | Stalker
    | etc.
data Building
    = Nexus
    | Pylon
    | Gateway
    | etc.
data Upgrade
    = WarpGate
    | AttackLv1
    | AttackLv2
    | AttackLv3
    | DefenseLv1
    | etc.
type Minerals = Int
type Gas = Int
type Time = Int
data Resources = Resources { minerals :: Minerals, gas :: Gas }

unitCost :: Unit -> Resources
unitCost u
    | Probe <= Resources { minerals = 50, gas = 0 }
    | Zealot <= Resources { minerals = 100, gas = 0 }
    | Stalker <= etc. etc.

buildingCost :: Building -> Resources
bulidingCost b
    | Nexus <= Resources { minerals = 400, gas = 0 }
    | Pylon <= etc. etc.

upgradeCost :: Upgrade -> Resources
upgradeCost u
    | WarpGate <= etc. etc.

unitBuildtime :: Unit -> Time
buildingBuildTime :: Building -> Time

So with that out of the way, we need to define our state.

Our state can entirely be defined in a couple of ways.

The first way would be to take our state as a big record. It would be the
number of minerals in the bank, the number of gas in the bank, a list of all
alive buildings, a list of all production buildings, a list of all alive units,
a list of all production units, again with upgrades, resource collection
allocation, and so forth.

No matter in what way we represent state, we are going to need a "command" list
as well. A command will be something like

data Command
    = Build Unit
    | Construct Building
    | Begin Upgrade
    | Reallocate Resources

Then, the way we represent the command stack has a few equivalent methods. The
first that comes to mind is a function from time to [Command].

type commandQueue = Time -> [Command]

So if a time t does not have any commands associated with it, then the
[command] list will be empty, but if a time t does have a command or commands
associated with it, then the list of commands will probide for it.

The other ways to do this are purely implementational in nature.

buildCommandQueue :: commandQueue -> (Time, Command) -> commandQueue

I like this function because it allows me to make rawData :: [(Time, Command)]
and say

foldl buildCommandQueue (\_ => []) rawData

And out comes my commandQueue. This is the beauty of functional programming.

So with the commandQueue, we are now able to talk about the heart of how this
thing is going to work. 

In our first representation of state, where we talk about the state as a
function from Time -> Some big ass record, we would write some function to be
our "step iterator" function.

Iterator :: commandQueue -> State -> Time -> State

What this function will do is take a Time, the State of the starcraft2 game at
that time, along with the commandQueue, and return the state one second in time
later. What an approach like this allows us to do is define our Time -> State
function like

findState commandQueue t =
    foldl (Iterator commandQueue) initState [0..t]

So this is all well and good, but what it means is that effectively what we are
doing is creating something like linear time complexity in just the operation
of finding the state of the game at a certain time.

I want to make note that this is how the built in SC2 replay program works. You
must play through the game in order to know the state at a certain point.

The other approach to capturing the state of the game would be to simply take
the command queue, and an item t, and return the State. In other words, this is
writing the findState function directly.

Now, in order for this to make sense, we must reorganize our command queue. It
no longer makes sense to keep it as a function from Time -> [Command]. Instead,
we will keep it in the form of our buildCommandQueue argument.

type commandQueue = [(Time, Command)]

Now, suppose we are at time 278, and we decide that we want to know how many
gateways we have. This would be purely a query on the commandQueue. We count
the number of elements in the queue where the command is (Build Gateway) and
the time of that command is less than 278 - 32 (32 being the gateway build
time) and that number is the number of gateways that have been built. From
this, we can then create the state directly, without having to build the state
iteratively at each step.

There are a couple things to mention here. The first is that every invocation
of "Build Gateway" might not result in a built gateway. That is, the result of
that action could be "You require more minerals" and so that action doesn't end
in a gateway being put in to production. The second is that something like the
number of minerals in the bank is more difficult to find based upon a query
such as this. It can be done, but it's more difficult to do so in a way that is
consistent with our iterative approach.

Both of these difficulties can be overcome, but we must be made aware of them.

So the first difficulty can be solved by making the assumption that the command
queue doesn't have any commands that cannot be executed. This could be done by
having something like a "prune command queue" function, which would take a
command queue and prune it of all the invalid entries. Then, we could wrap the
command queue around a type that contains information about whether or not the
command queue has been pruned. So if elements are added to a pruned command
queue, the command queue wrapper will be flagged as unpruned until the pruning
function is applied to the command queue again. This is similar to the way that
index out of bounds is handled in an array list.

That is it for the first difficulty. As I think more upon something like the
number of minerals in the bank, it seems to me that, for the purposes of
representing the state, that number should be continuous. The reasoning for
this is so that a collection rate can be placed in, and every second, the
number of minerals would equal the current number of minerals plus the number
of probes gathering minerals times the collection rate (which would neccesarily
be a continuous number). This allows us to get around the problems of rounding
when it comes to using integers. We just won't round at any point, except only
as the very last action if neccesary.

So the next thing to figure out is, given a command queue, we can figure out as
a function of time the number of probes that are on minerals at any given time.
From this number, and the fact that we start with 12 probes and 50 minerals, we
can calculate our total accumulated resources. How to do this is the next thing
we need to figure out.

Before we do that, let's talk about the easier version. Given a pruned command
queue, we can very easily find out how much we have spent up to a certain
point. But we have yet to talk about the "pruning process" and it's in fact
this process of pruning which gets us to our final values.

The only way to really calculate this is through pruning, and the only way you
can truly prune is iteratively. Unfortunately, there is no shortcut around this
part.

So again, let's just make the assumption that we are given a pruned command
queue. The total resources gathered at a point in time t will be the sum of all
t of the resource collection rate times the number of probes on minerals at
each point t.

MineralProbeCount :: CommandQueue -> Time -> Int
totalMinerals :: CommandQueue -> Time -> Minerals
totalMinerals cq t = resourceCollectionRate * sum $ [0..t] <$> MineralProbeCount cq

So in other words, we still have to sum up the collection rate across the
entire stretch of time.

However, this has the advantage over the iterative approach that we are not
keeping track of everything. That is, once we have pruned the Command Queue, we
just need to know the function from Time to ProbeCount, which we can get by
querying the queue, and then we can calculate the total minerals gathered in
that time as a pure query of the Command Queue.

Likewise, we can do something similar with a pruned queue and resources spent
in a given time. We could create something of a helper function for this
function that is a Command -> Resources function, which tells us how much
resources a certain command would cost. With that, we can then create our
function as the sum

ResourcesSpent :: CommandQueue -> Time -> Resources
ResourcesSpent cq t = sum $ Helper <$> [a | (a, i) <- cq, i < t]

From this, we should be able to do two things. First, we should be able to
prove that the CommandQueue is pruned iff totalResources - spentResources >= 0
for all t. But beyond that, what we can do is give the resources in the bank as
the subtraction of these two functions.

So, as you will notice, every thing that we have wanted to calculate about the
state we have been able to do by querying the (Command, Time) list. Since the
state of the game in the present does not depend on the state of the game in
the future, when we are calculating some aspect of the state, we can take the
subset of our Command Queue where time is less than or equal to the given time.
Because of this, instead of calculating the entire state at one time, we can
calculate just one aspect of the state. That is the main advantage of this.

So when we want to know the number of zealots in our command queue, we first
check if the queue is pruned. This is constant time. If it is pruned then we
can perform some query on the command queue to get our desired result. This
query will always take at most t iterations to get the desired aggregate
quantity. Supposing there are n elements of the state that are worth keeping
track of, the best case working scenario for our iterative model is O(nt) where
as the best case working scenario for our query based model is O(t). Notice
that our iterative model might be way worse than that, because certain kinds of
elements of state might be very complicated, and depend on things like sorting
other elements of state, or something else equally erratic. In those cases, the
best case scenario for our query based model would be O(t^2) (let's just say
it's quadratic) and the best case scenario for our iterative model would be
O((n-1)t + t^2) = O(nt^2). So it seems like we haven't done anything. But when
this feature of state is kept track of in our state diagram, then we have, for
our other state features, the best case in our first scenario to be O(t) but
O(nt^2) in our second scenario. So we have gotten even worse performance now
than before for those elements of the state which shouldn't be dependent on our
complicated state property.

So I really want to begin to write this right now, because I think I have most
of the things I need in place, but I am at work, and I know that if I start
writing this, I won't stop. So I am going to stop there for now, and continue
on with my life.


