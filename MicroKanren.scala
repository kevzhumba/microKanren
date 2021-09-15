package microKanren

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object MicroKanren {

  type Goal = State => Stream 
  type Subst = Map[Var, Val]

  abstract trait Val
  case class Var(index: Int) extends Val
  case class IntWrapper(value: Int) extends Val
  case class ListWrapper(list: List[Val]) extends Val

  abstract trait Stream
  case class MatureStream(state: State, stream: Stream) extends Stream
  case class ImmatureStream(func: () => Stream) extends Stream
  case class EmptyStream() extends Stream

  case class State(s: Subst, c: Int)
  case class UnificationException() extends Exception

  def EmptyState(): State = State(Map(), 0)

  def walk(u: Val, subst: Subst): Val = {
    if (u.isInstanceOf[Var] && subst.contains(u.asInstanceOf[Var])) walk(subst((u.asInstanceOf[Var])), subst) else u
  }

  def extendSubst(x: Var, v: Val, s: Subst): Subst = s + (x -> v)

  def unify(a: Val, b: Val, s: Subst): Subst = (walk(a, s), walk(b, s)) match {
    case (Var(index1), Var(index2)) if (index1 == index2) => s
    case (x: Var, v) => extendSubst(x, v, s)
    case (v, x: Var) => extendSubst(x, v, s)
    case (ListWrapper(list1), ListWrapper(list2)) if list1.length == list2.length => 
      if (list1.isEmpty) s
      val newS = unify(list1.head, list2.head, s) 
      unify(ListWrapper(list1.tail), ListWrapper(list2.tail), newS)
    case (u, v) if u == v => s
    case (_, _) => throw UnificationException()
  }

  def call_fresh(f: Var => Goal): Goal = {
    (state: State) => f(Var(state.c))(state.copy(c = state.c + 1))
  }

  def equality(a: Val, b: Val): Goal = {
    (state: State) => try {
      MatureStream(State(unify(a, b, state.s), state.c), EmptyStream())
    } catch {
      case UnificationException() => EmptyStream()
    } 
  }

  def mplus(s1: Stream, s2: Stream): Stream = (s1, s2) match {
    case (EmptyStream(), _) => s2
    case (ImmatureStream(func), _) => ImmatureStream(() => mplus(s2, func()))
    case (MatureStream(state, stream), _) => MatureStream(state, mplus(stream, s2))
  }

  def bind(s1: Stream, g: Goal): Stream = s1 match {
    case EmptyStream() => EmptyStream()
    case ImmatureStream(func) => ImmatureStream(() => bind(func(), g))
    case MatureStream(state, stream) => mplus(g(state),  bind(stream, g))
  }

  def disj(g1: Goal, g2: Goal): Goal = {
    (state: State) => mplus(g1(state), g2(state))
  }

  def conj(g1: Goal, g2: Goal): Goal = { 
    (state: State) => bind(g1(state), g2)
  }

  def zzz(g: Goal): Goal = {
    (s: State) => ImmatureStream(() => g(s))
  }

  def conjMult(goals: Goal*): Goal = {
    goals match {
      case g :: Nil => zzz(g)
      case g :: gs => conj(zzz(g), conjMult(gs: _*))
    }
  }

  def disjMult(goals: Goal*): Goal = {
    goals match {
      case g :: Nil => zzz(g)
      case g :: gs => disj(zzz(g), disjMult(gs: _*))
    }
  }

  def pull(stream: Stream): Stream = stream match {
    case ImmatureStream(func) => pull(func())
    case _ => stream
  }

  def take_all(stream: Stream): List[State] = pull(stream) match {
    case EmptyStream() => List()
    case MatureStream(state, tailStream) =>  state :: take_all(tailStream)
  }

  def take_n(n: Int, stream: Stream): List[State] =
    if (n <= 0) {List()}
    else {
      pull(stream) match {
        case EmptyStream() => List()
        case MatureStream(state, tailStream) => state :: take_n(n - 1, tailStream)
      }
    }
}