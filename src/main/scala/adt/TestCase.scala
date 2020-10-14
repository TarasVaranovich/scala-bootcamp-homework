package adt

object TestCase {
  val texasHoldem = GameService.playGame(Hand.Texas.rules) _
  val texasResult = texasHoldem.apply(List.empty, List.empty)
  GameService.judgeResult(texasResult)

  val omahaHoldem = GameService.playGame(Hand.Omaha.rules) _
  val omahaResult = omahaHoldem.apply(List.empty, List.empty)
  GameService.judgeResult(omahaResult)
}