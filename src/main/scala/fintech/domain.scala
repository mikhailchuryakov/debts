package fintech

import java.util.UUID

import io.circe.derivation.annotations.JsonCodec
import cats.implicits._

@JsonCodec
final case class Group(id: UUID, name: String, users: List[User]) {
  def addUser(name: String): Group = {
    val newUserID = UUID.randomUUID()
    val newUser = User(newUserID, name, users.map(user => Tie(user.id, 0.0)))
    println(s"Adding user with name $name") // todo delete
    copy(users = newUser :: users.map(user => User(user.id, user.name, Tie(newUserID, 0.0) :: user.ties)))
  }

  def removeUser(id: UUID): Group = copy(users = users.filter(user => user.id != id)) // todo either delete ties

  def pay(fromID: UUID, value: Double, betweenIDs: Seq[UUID]): Group = {
    val userFromOld = users.filter(_.id == fromID).head
    val userFromNew = User(userFromOld.id, userFromOld.name, userFromOld.ties.map(tie => betweenIDs.find(_ == tie.idTo) match {
      case Some(a) => Tie(a, tie.value + value)
      case _ => tie
    }))
    val usersToNew = betweenIDs.map(userID => {
      val userToOld = users.filter(_.id == userID).head
      User(userToOld.id, userToOld.name,
        userToOld.ties.map(tie => if (tie.idTo == fromID) Tie(fromID, tie.value - value) else tie))
    }).toList
    val allUsers = users.flatMap(user => usersToNew.map(userTo => {
      if (user.id == userTo.id) userTo
      else if (user.id == userFromNew.id) userFromNew
      else user
    }))
    copy(users = allUsers)
  }

}

object Group {
  implicit val key: Key[Group] = new Key[Group] {
    def key(a: Group): UUID = a.id

    def notFound(uuid: UUID): Throwable = GroupNotFound(uuid)

    def multiple(uuid: UUID): Throwable = MultipleGroups(uuid)
  }
}

@JsonCodec
final case class User(id: UUID, name: String, ties: List[Tie])

object User {
  implicit val key: Key[User] = new Key[User] {
    def key(a: User): UUID = a.id

    def notFound(uuid: UUID): Throwable = UserNotFound(uuid)

    def multiple(uuid: UUID): Throwable = MultipleUsers(uuid)
  }
}

@JsonCodec
final case class Tie(idTo: UUID, value: Double)

object Tie {
  implicit val key: Key[User] = new Key[User] { // todo Change exception
    def key(a: User): UUID = a.id

    def notFound(uuid: UUID): Throwable = UserNotFound(uuid)

    def multiple(uuid: UUID): Throwable = MultipleUsers(uuid)
  }
}

@JsonCodec
final case class CreateGroup(name: String)

final case class CreateUser(name: String,
                            //                            ties: List[Double]
                           )

abstract class StacklessException(message: String) extends Exception(message, null, false, false)

final case class GroupNotFound(id: UUID) extends StacklessException(s"Group $id not found")

final case class MultipleGroups(id: UUID) extends StacklessException(s"Multiple groups with id $id")

final case class UserNotFound(id: UUID) extends StacklessException(s"User $id not found")

final case class MultipleUsers(id: UUID) extends StacklessException(s"Multiple users with id $id")