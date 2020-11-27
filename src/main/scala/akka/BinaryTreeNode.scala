package akka

import akka.BinaryTreeSet.OperationReply.{ContainsResult, OperationFinished}
import akka.BinaryTreeSet.{Operation, OperationReply}
import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet.Operation._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case insert: Insert => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Remove => doRemove(remove)
  }

  private def doInsert(m: Insert): Unit =
    treatCommand(m, insertSubNode, () => removed = false, operation => OperationFinished(operation.id))

  private def doContains(m: Contains): Unit =
    treatCommand(m, _ => (), () => (), operation => solveContains(operation))

  private def doRemove(m: Remove): Unit =
    treatCommand(m, operation => operation.requester ! OperationFinished(operation.id),
      () => removed = true, operation => OperationFinished(operation.id))

  private def treatCommand(operation: Operation, execute: Operation => Unit,
                           terminate: () => Unit, reply: (Operation) => OperationReply): Unit =
    if (operation.elem == elem) {
      terminate()
      operation.requester ! reply(operation)
    }
    else if (subtrees.contains(solvePosition(operation))) {
      subtrees(solvePosition(operation)) ! operation
    } else {
      execute(operation)
      operation.requester ! reply(operation)
    }

  private val insertSubNode: Operation => Unit = (operation: Operation) => {
    val node = context.actorOf(BinaryTreeNode.props(operation.elem, false))
    subtrees += (solvePosition(operation) -> node)
  }

  private val solveContains: Operation => ContainsResult = (operation: Operation) => {
    val result = if (operation.elem == elem) !removed else false
    ContainsResult(operation.id, result)
  }

  private val solvePosition: Operation => Position = (operation: Operation) =>
    if (operation.elem < elem) Left else Right
}