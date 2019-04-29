package fintech

import java.util.UUID

import io.circe.derivation.annotations.JsonCodec
import cats.implicits._

@JsonCodec
final case class Group(id: UUID, name: String, users: List[User]) {
  def addUser(name: String): Group = {
    println(s"Adding user with name $name") // todo delete
    copy(users = User(UUID.randomUUID(), name, List()) :: users)
  }

  def removeUser(id: UUID): Group = copy(users = users.filter(user => user.id != id))
}
object Group {
  implicit val key: Key[Group] = new Key[Group] {
    def key(a: Group): UUID = a.id
    def notFound(uuid: UUID): Throwable = GroupNotFound(uuid)
    def multiple(uuid: UUID): Throwable = MultipleGroups(uuid)
  }
}

@JsonCodec
final case class User(id: UUID, name: String, ties: List[Double])
object User {
  implicit val key: Key[User] = new Key[User] {
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

final case class GroupNotFound(id: UUID)    extends StacklessException(s"Group $id not found")
final case class MultipleGroups(id: UUID)   extends StacklessException(s"Multiple groups with id $id")
final case class UserNotFound(id: UUID)     extends StacklessException(s"User $id not found")
final case class MultipleUsers(id: UUID)    extends StacklessException(s"Multiple users with id $id")