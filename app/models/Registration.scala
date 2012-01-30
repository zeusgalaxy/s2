package models

import java.util.{Date}
import xml.Elem

//* http://qa-ec2.netpulse.ws/core/n5iregister.jsp?machine_id=18&id=5103369779&membership_id=1&email=kenner%40stross.com
//& pic=22&DOB=01012001&gender=M&enableMail=1&weight=180&oem_tos=15

//http://localhost:9000/register?machine_id=18&id=5103369779&membership_id=1&email=kenner%40stross.com&pic=22&DOB=01012001&gender=M&enableMail=1&weight=180&oem_tos=15

//case class Registration(machineId: Int)
case class Registration(machineId: Int, id: Long, membershipId: Option[Int], email: String,
                        pic: Option[Int], dob: Date,
                        gender: String, enableMail: Boolean, weight: Int, oemTos: Option[Int])

object Registration {

  def test(registration: Registration) : Elem = {
    <registeredEmail>{registration.email}</registeredEmail>
  }

  def appendVTUser : Elem = {
    <nouser>No User Found</nouser>
  }
}