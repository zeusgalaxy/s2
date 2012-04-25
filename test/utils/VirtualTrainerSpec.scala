package test.controllers

import utils._
import services._
import controllers._
import models._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.{EquipmentDao, Equipment, MachineDao, MachineBasic}
import play.api.libs.ws.WS
import xml.Elem

object VirtualTrainerSpec extends Specification {

  val id = org.joda.time.DateTime.now.getMillis.toString.takeRight(10)
  val pwd = id
  val email = "kgs" + id + "@stross.com"
  val channel = (Math.random * 100).round

  trait FakeMachineDao extends MachineDao with EquipmentDao {
    override def mchGetBasic(id: Long): Option[MachineBasic] = Some(MachineBasic(1070L, 99, "99"))
    override def mchGetWithEquip(id: Long): Option[(MachineBasic, Option[Equipment])] =
        Some(MachineBasic(1070L, 99, "99"), Some(Equipment(88L, 16, 99, Some("etype"),
              Some("LifeFitness"), Some("Bicycle"))))
  }

  case class FakeVtResponse(override val status: Int = 200, override val xml: Option[Elem] = None,
                             headers: Map[String, String] = Map[String, String](),
                             override val body: String = "") extends VtResponse {
    override def header(key: String) = headers.get(key)
  }

  val vtr = FakeVtResponse()
  val vtrValidateFound = FakeVtResponse(status = 500)
  val vtrValidateNotFound = FakeVtResponse(status = 200)
  val vtrValidateError = FakeVtResponse(status = 404)

  class FakeDinoController extends DinoController
                          with VirtualTrainer
                          with ExerciserDao
                          with MachineDao
                          with EquipmentDao
                          with PageViewDao

  case class FakeVirtualTrainer(validate: VtResponse = vtr, register: VtResponse = vtr, login: VtResponse = vtr,
                logout: VtResponse = vtr, link: VtResponse = vtr, predefineds: VtResponse = vtr, workouts: VtResponse = vtr)
        extends VirtualTrainer with FakeMachineDao with EquipmentDao {

    override def vtDoValidate(rBody: String) = validate
    override def vtDoRegister(rBody: String) = register
    override def vtDoLogin(lBody: String) = login
    override def vtDoLogout(token: String, tokenSecret: String) = logout
    override def vtDoLink(npLogin: String, vtUid: String) = link
    override def vtDoPredefineds(token: String, tokenSecret: String) = predefineds
    override def vtDoWorkouts(token: String, tokenSecret: String) = workouts
  }

  val vtrRegParams = VtRegistrationParams(npLogin = "5103369779", email = "123@netpulse.com", dob = "01021963",
          machineId = "1070", gender = "M", weight = "180", vtNickname = "usertest", vtPassword = "5103369779")

  "VirtualTrainer" should {
    "Handle all possibilities when validating prior to registration" in {

      running(FakeApplication()) {
        val result = FakeVirtualTrainer(validate = FakeVtResponse(status = 500)).vtRegister(vtrRegParams)
        result must equalTo(Left(apiVtRegistrationUserExists))

//        val result2 = FakeVirtualTrainer(register = FakeVtResponse(xml = Some(<weird></weird>))).vtRegister(vtrRegParams)
//        result2 must equalTo(Left(apiVtRegistrationUserExists))
      }
    }
//    "make all of its calls properly" in {
//
//      running(FakeApplication()) {
//
//        var pth = "http://localhost:9000/n5iregister.jsp?machine_id=1070&id=" + id + "&membership_id=1&email=" + email + "&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15"
//        var result = controllers.DinoController.n5iRegister()(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("adunit")
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        pth = "http://localhost:9000/n5ilogin.jsp?machine_id=1070&id=" + id + "&pic=22&oem_tos=15"
//        result = controllers.DinoController.n5iLogin(id, 1070L)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("adunit")
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        /**
//         * http://localhost:9000/exerciserStatus?id=2115180443
//         * http://localhost:9000/getChannels?id=2115180443,location_id=87
//         *
//         * setChannels:
//         *         An example call to test locally, when in the test/controllers directory:
//         *         curl --header "Content-Type: text/xml; charset=UTF-8" -d@setChannels.xml http://localhost:9000/setChannels
//         *
//         */
//
//        pth = "http://localhost:9000/vtLogin?machine_id=1070&id=" + id + "&vt_password=" + pwd
//        result = controllers.ApiController.apiVtLogin(id, pwd, 1070L)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        pth = "http://localhost:9000/vtLogout?id=" + id
//        result = controllers.ApiController.apiVtLogout(id)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//
//        pth = "http://localhost:9000/vtRegister?machine_id=1070&id=" + id
//        result = controllers.ApiController.apiVtRegister(id, 1070L)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("api error=\"2\"") // They're already registered with vt!
//
//        pth = "http://localhost:9000/exerciserStatus?id=" + id
//        result = controllers.ApiController.apiExerciserStatus(id)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("homeClub")
//        contentAsString(result) must contain("email")
//
//        pth = "http://localhost:9000/setChannels"
//        // TODO - Need a real record that we can pollute with our test data
//        val cnt = <channels npLogin='s2' locationId='99'>
//          <channel>{channel}</channel>
//        </channels>
//
//        val fr = FakeRequest("POST", pth, FakeHeaders(Map("Content-Type" -> List("text/xml"))), cnt)
//        result = controllers.ApiController.apiSetChannels()(fr)
//
//        //        status(result) must equalTo(OK)
//        //        contentAsString(result) must contain("api error=\"0\"")
//
//        pth = "http://localhost:9000/getChannels?id=s2&location_id=99"
//        result = controllers.ApiController.apiGetChannels("s2", 99L)(FakeRequest("GET", pth))
//
//        //        status(result) must equalTo(OK)
//        //        contentAsString(result) must contain("api error=\"0\"")
//        //        contentAsString(result) must contain(channel.toString)
//
//        pth = "http://localhost:9000/vtLinkUser?machine_id=1070&id=" + id + "&vt_password=" + pwd
//        result = controllers.ApiController.apiVtLinkUser(id, pwd, 1070L)(FakeRequest("GET", pth))
//
//        /**
//         * TODO -- Testing the account linking feature requires more setup than this. We would need to
//         * create accounts on both Virtual Trainer and Netpulse -- unlinked -- and then link them with
//         * this call. That setup would need to be redone each time we run this test, since you cannot link
//         * accounts that have already been linked. For now, we're just testing to see that we get the
//         * failure we expect (which really doesn't even tell us if it's TRULY the failure we're expecting;
//         * for that we'd need to look at the log output).
//         */
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("<api error=\"0\"")
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        pth = "http://localhost:9000/gigyaProxy/notifyLogin?siteUID=" + id + "&newUser=true"
//        result = controllers.ApiController.apiGigyaProxy("notifyLogin")(FakeRequest("GET", pth))
//
//        contentAsString(result) must contain("UIDSignature")
//      }
//    }
  }
}

object VirtualTrainerData {

  val registerXml =
    <ns2:account xmlns:ns2="http://api.virtualtrainer.lifefitness.com/v1/account"><age>52</age><emailAddress>0002@stross.com</emailAddress><emailNotificationAgreement>false</emailNotificationAgreement><gender>m</gender><hasFbSharing>false</hasFbSharing><heightUnit>I</heightUnit><locationId>90</locationId><nickName>6660010002</nickName><preferredLanguageCode>en_US</preferredLanguageCode><userId>93328</userId><weight>180.0</weight><weightUnit>I</weightUnit></ns2:account>
  val loginXml =
    <ns2:account xmlns:ns2="http://api.virtualtrainer.lifefitness.com/v1/account"><age>52</age><emailAddress>0002@stross.com</emailAddress><emailNotificationAgreement>false</emailNotificationAgreement><gender>m</gender><hasFbSharing>false</hasFbSharing><heightUnit>I</heightUnit><locationId>90</locationId><nickName>6660010002</nickName><preferredLanguageCode>en_US</preferredLanguageCode><userId>93328</userId><weight>180.0</weight><weightUnit>I</weightUnit></ns2:account>
  val predefinedsXml =
    <ns2:predefinedPresets xmlns:ns2="http://api.virtualtrainer.lifefitness.com/v1/programtemplate"><workoutSegments><workoutId>9</workoutId><workoutName>Manual</workoutName><equipment>Recumbent Bike</equipment><time>30.0</time><level>2.0</level><goal>4</goal><deviceType>12</deviceType><deviceType>30</deviceType><deviceType>32</deviceType><deviceType>31</deviceType><filename>Recumbent Bike 30 min Workout 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTc6MTU6MDYrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxNzoxNTowNiswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjMyPC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT40PC9wcm9ncmFtLXR5cGU+Cjxnb2FsLXR5cGU+MTwvZ29hbC10eXBlPgo8Z29hbC12YWx1ZT4xODAwLjA8L2dvYWwtdmFsdWU+CjxsZXZlbD4yLjA8L2xldmVsPgo8L3dvcmtvdXQtcHJlc2V0Pgo8dml2by1ibG9jaz4KPHVzZXItaWQ+NTE8L3VzZXItaWQ+Cjxhc3NpZ25lZC1wcm9nLWlkPjgzMTQ5PC9hc3NpZ25lZC1wcm9nLWlkPgo8YWN0aXZpdHktaWQ+MjA5OTQyPC9hY3Rpdml0eS1pZD4KPGFjdGl2aXR5LWRlZi1pZD4zPC9hY3Rpdml0eS1kZWYtaWQ+Cjx3b3Jrb3V0LWlkPjE8L3dvcmtvdXQtaWQ+Cjx3b3Jrb3V0LXNlc3MtaWQ+MTwvd29ya291dC1zZXNzLWlkPgo8bG9jYXRpb24taWQ+MzE3PC9sb2NhdGlvbi1pZD4KPHNvdXJjZT4wPC9zb3VyY2U+Cjwvdml2by1ibG9jaz4KPC93b3Jrb3V0Pg==</presetFile></workoutSegments><workoutSegments><workoutId>10</workoutId><workoutName>Manual</workoutName><equipment>Treadmill</equipment><distance>10.0</distance><incline>0.5</incline><initialSpeed>2.49999999999866</initialSpeed><goal>1</goal><deviceType>4</deviceType><deviceType>14</deviceType><deviceType>18</deviceType><deviceType>21</deviceType><deviceType>22</deviceType><deviceType>23</deviceType><deviceType>1</deviceType><deviceType>41</deviceType><filename>Treadmill 10K Saturday 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MDU6MjcrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOTowNToyNyswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjIzPC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT40PC9wcm9ncmFtLXR5cGU+Cjxnb2FsLXR5cGU+MjwvZ29hbC10eXBlPgo8Z29hbC12YWx1ZT4xMC4wPC9nb2FsLXZhbHVlPgo8Z29hbC11bml0PjE8L2dvYWwtdW5pdD4KPHNwZWVkIHVuaXRzPSIxIj4yLjQ5OTk5OTk5OTk5ODY2PC9zcGVlZD4KPGluY2xpbmU+MC41PC9pbmNsaW5lPgo8L3dvcmtvdXQtcHJlc2V0Pgo8dml2by1ibG9jaz4KPHVzZXItaWQ+NTE8L3VzZXItaWQ+Cjxhc3NpZ25lZC1wcm9nLWlkPjgzMTYzPC9hc3NpZ25lZC1wcm9nLWlkPgo8YWN0aXZpdHktaWQ+MjA5OTYxPC9hY3Rpdml0eS1pZD4KPGFjdGl2aXR5LWRlZi1pZD4xPC9hY3Rpdml0eS1kZWYtaWQ+Cjx3b3Jrb3V0LWlkPjE8L3dvcmtvdXQtaWQ+Cjx3b3Jrb3V0LXNlc3MtaWQ+MTwvd29ya291dC1zZXNzLWlkPgo8bG9jYXRpb24taWQ+MzE3PC9sb2NhdGlvbi1pZD4KPHNvdXJjZT4wPC9zb3VyY2U+Cjwvdml2by1ibG9jaz4KPC93b3Jrb3V0Pg==</presetFile></workoutSegments><workoutSegments><workoutId>8</workoutId><workoutName>Hill</workoutName><equipment>Treadmill</equipment><time>30.0</time><level>4.0</level><initialSpeed>2.414016</initialSpeed><goal>4</goal><deviceType>4</deviceType><deviceType>14</deviceType><deviceType>18</deviceType><deviceType>21</deviceType><deviceType>22</deviceType><deviceType>23</deviceType><deviceType>1</deviceType><deviceType>41</deviceType><filename>Hill Workout - Treadmill 02.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTc6MTM6NDIrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxNzoxMzo0MiswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjIzPC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4yPC9wcm9ncmFtLXR5cGU+Cjxnb2FsLXR5cGU+MTwvZ29hbC10eXBlPgo8Z29hbC12YWx1ZT4xODAwLjA8L2dvYWwtdmFsdWU+CjxsZXZlbD40LjA8L2xldmVsPgo8c3BlZWQgdW5pdHM9IjAiPjEuNTwvc3BlZWQ+Cjwvd29ya291dC1wcmVzZXQ+Cjx2aXZvLWJsb2NrPgo8dXNlci1pZD41MTwvdXNlci1pZD4KPGFzc2lnbmVkLXByb2ctaWQ+ODMxNDg8L2Fzc2lnbmVkLXByb2ctaWQ+CjxhY3Rpdml0eS1pZD4yMDk5NDE8L2FjdGl2aXR5LWlkPgo8YWN0aXZpdHktZGVmLWlkPjE8L2FjdGl2aXR5LWRlZi1pZD4KPHdvcmtvdXQtaWQ+Mzwvd29ya291dC1pZD4KPHdvcmtvdXQtc2Vzcy1pZD4xPC93b3Jrb3V0LXNlc3MtaWQ+Cjxsb2NhdGlvbi1pZD4zMTc8L2xvY2F0aW9uLWlkPgo8c291cmNlPjA8L3NvdXJjZT4KPC92aXZvLWJsb2NrPgo8L3dvcmtvdXQ+</presetFile></workoutSegments><workoutSegments><workoutId>11</workoutId><workoutName>Fat Burn</workoutName><equipment>Treadmill</equipment><calories>150.0</calories><heartRate>134.0</heartRate><initialSpeed>3.218688</initialSpeed><goal>2</goal><deviceType>4</deviceType><deviceType>14</deviceType><deviceType>18</deviceType><deviceType>21</deviceType><deviceType>22</deviceType><deviceType>23</deviceType><deviceType>1</deviceType><deviceType>41</deviceType><filename>Tuesday's Fat Burn Treadmill 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MDc6NDgrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOTowNzo0OCswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjIzPC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT41PC9wcm9ncmFtLXR5cGU+Cjxnb2FsLXR5cGU+MzwvZ29hbC10eXBlPgo8Z29hbC12YWx1ZT4xNTAuMDwvZ29hbC12YWx1ZT4KPGdvYWwtdW5pdD4wPC9nb2FsLXVuaXQ+Cjx0YXJnZXQtaHI+MTM0LjA8L3RhcmdldC1ocj4KPHNwZWVkIHVuaXRzPSIwIj4yLjA8L3NwZWVkPgo8L3dvcmtvdXQtcHJlc2V0Pgo8dml2by1ibG9jaz4KPHVzZXItaWQ+NTE8L3VzZXItaWQ+Cjxhc3NpZ25lZC1wcm9nLWlkPjgzMTY2PC9hc3NpZ25lZC1wcm9nLWlkPgo8YWN0aXZpdHktaWQ+MjA5OTY0PC9hY3Rpdml0eS1pZD4KPGFjdGl2aXR5LWRlZi1pZD4xPC9hY3Rpdml0eS1kZWYtaWQ+Cjx3b3Jrb3V0LWlkPjQ8L3dvcmtvdXQtaWQ+Cjx3b3Jrb3V0LXNlc3MtaWQ+MTwvd29ya291dC1zZXNzLWlkPgo8bG9jYXRpb24taWQ+MzE3PC9sb2NhdGlvbi1pZD4KPHNvdXJjZT4wPC9zb3VyY2U+Cjwvdml2by1ibG9jaz4KPC93b3Jrb3V0Pg==</presetFile></workoutSegments><workoutSegments><workoutId>1</workoutId><workoutName>Random</workoutName><equipment>Treadmill</equipment><distance>5.1499008</distance><level>3.0</level><initialSpeed>4.02336</initialSpeed><goal>1</goal><deviceType>4</deviceType><deviceType>14</deviceType><deviceType>18</deviceType><deviceType>21</deviceType><deviceType>22</deviceType><deviceType>23</deviceType><deviceType>1</deviceType><deviceType>41</deviceType><filename>5K Training - Treadmill 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MjA6MTgrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOToyMDoxOCswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjIzPC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4zPC9wcm9ncmFtLXR5cGU+Cjxnb2FsLXR5cGU+MjwvZ29hbC10eXBlPgo8Z29hbC12YWx1ZT4zLjI8L2dvYWwtdmFsdWU+CjxsZXZlbD4zLjA8L2xldmVsPgo8Z29hbC11bml0PjA8L2dvYWwtdW5pdD4KPHNwZWVkIHVuaXRzPSIwIj4yLjU8L3NwZWVkPgo8L3dvcmtvdXQtcHJlc2V0Pgo8dml2by1ibG9jaz4KPHVzZXItaWQ+NTE8L3VzZXItaWQ+Cjxhc3NpZ25lZC1wcm9nLWlkPjgzMTc2PC9hc3NpZ25lZC1wcm9nLWlkPgo8YWN0aXZpdHktaWQ+MjA5OTc0PC9hY3Rpdml0eS1pZD4KPGFjdGl2aXR5LWRlZi1pZD4xPC9hY3Rpdml0eS1kZWYtaWQ+Cjx3b3Jrb3V0LWlkPjU8L3dvcmtvdXQtaWQ+Cjx3b3Jrb3V0LXNlc3MtaWQ+MTwvd29ya291dC1zZXNzLWlkPgo8bG9jYXRpb24taWQ+MzE3PC9sb2NhdGlvbi1pZD4KPHNvdXJjZT4wPC9zb3VyY2U+Cjwvdml2by1ibG9jaz4KPC93b3Jrb3V0Pg==</presetFile></workoutSegments><workoutSegments><workoutId>4</workoutId><workoutName>Aerobics</workoutName><equipment>Cross-Trainer</equipment><distance>4.02336</distance><level>11.0</level><goal>1</goal><deviceType>5</deviceType><deviceType>6</deviceType><deviceType>16</deviceType><deviceType>17</deviceType><deviceType>24</deviceType><deviceType>26</deviceType><deviceType>25</deviceType><deviceType>2</deviceType><deviceType>43</deviceType><filename>Cardio Blast Cross-Trainer 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MTc6MzcrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOToxNzozNyswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjI2PC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4xMjwvcHJvZ3JhbS10eXBlPgo8Z29hbC10eXBlPjI8L2dvYWwtdHlwZT4KPGdvYWwtdmFsdWU+Mi41PC9nb2FsLXZhbHVlPgo8bGV2ZWw+MTEuMDwvbGV2ZWw+Cjxnb2FsLXVuaXQ+MDwvZ29hbC11bml0Pgo8L3dvcmtvdXQtcHJlc2V0Pgo8dml2by1ibG9jaz4KPHVzZXItaWQ+NTE8L3VzZXItaWQ+Cjxhc3NpZ25lZC1wcm9nLWlkPjgzMTc0PC9hc3NpZ25lZC1wcm9nLWlkPgo8YWN0aXZpdHktaWQ+MjA5OTcyPC9hY3Rpdml0eS1pZD4KPGFjdGl2aXR5LWRlZi1pZD40PC9hY3Rpdml0eS1kZWYtaWQ+Cjx3b3Jrb3V0LWlkPjY8L3dvcmtvdXQtaWQ+Cjx3b3Jrb3V0LXNlc3MtaWQ+MTwvd29ya291dC1zZXNzLWlkPgo8bG9jYXRpb24taWQ+MzE3PC9sb2NhdGlvbi1pZD4KPHNvdXJjZT4wPC9zb3VyY2U+Cjwvdml2by1ibG9jaz4KPC93b3Jrb3V0Pg==</presetFile></workoutSegments><workoutSegments><workoutId>6</workoutId><workoutName>Heart Rate Hill</workoutName><equipment>Cross-Trainer</equipment><distance>3.218688</distance><heartRate>152.0</heartRate><goal>1</goal><deviceType>5</deviceType><deviceType>6</deviceType><deviceType>16</deviceType><deviceType>17</deviceType><deviceType>24</deviceType><deviceType>26</deviceType><deviceType>25</deviceType><deviceType>2</deviceType><deviceType>43</deviceType><filename>Cross-Trainer Heart Rate Workout 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTc6MTc6NTArMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxNzoxNzo1MCswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjI2PC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT43PC9wcm9ncmFtLXR5cGU+Cjxnb2FsLXR5cGU+MjwvZ29hbC10eXBlPgo8Z29hbC12YWx1ZT4yLjA8L2dvYWwtdmFsdWU+Cjxnb2FsLXVuaXQ+MDwvZ29hbC11bml0Pgo8dGFyZ2V0LWhyPjE1Mi4wPC90YXJnZXQtaHI+Cjwvd29ya291dC1wcmVzZXQ+Cjx2aXZvLWJsb2NrPgo8dXNlci1pZD41MTwvdXNlci1pZD4KPGFzc2lnbmVkLXByb2ctaWQ+ODMxNTA8L2Fzc2lnbmVkLXByb2ctaWQ+CjxhY3Rpdml0eS1pZD4yMDk5NDM8L2FjdGl2aXR5LWlkPgo8YWN0aXZpdHktZGVmLWlkPjQ8L2FjdGl2aXR5LWRlZi1pZD4KPHdvcmtvdXQtaWQ+Nzwvd29ya291dC1pZD4KPHdvcmtvdXQtc2Vzcy1pZD4xPC93b3Jrb3V0LXNlc3MtaWQ+Cjxsb2NhdGlvbi1pZD4zMTc8L2xvY2F0aW9uLWlkPgo8c291cmNlPjA8L3NvdXJjZT4KPC92aXZvLWJsb2NrPgo8L3dvcmtvdXQ+</presetFile></workoutSegments><workoutSegments><workoutId>3</workoutId><workoutName>Around the world</workoutName><equipment>Upright Bike</equipment><time>15.0</time><level>10.0</level><goal>4</goal><deviceType>11</deviceType><deviceType>27</deviceType><deviceType>29</deviceType><deviceType>28</deviceType><deviceType>0</deviceType><deviceType>42</deviceType><filename>Beach Body 15 min Bike Workout 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MTM6MDErMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOToxMzowMSswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjI5PC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4yNzwvcHJvZ3JhbS10eXBlPgo8Z29hbC10eXBlPjE8L2dvYWwtdHlwZT4KPGdvYWwtdmFsdWU+OTAwLjA8L2dvYWwtdmFsdWU+CjxsZXZlbD4xMC4wPC9sZXZlbD4KPC93b3Jrb3V0LXByZXNldD4KPHZpdm8tYmxvY2s+Cjx1c2VyLWlkPjUxPC91c2VyLWlkPgo8YXNzaWduZWQtcHJvZy1pZD44MzE3MzwvYXNzaWduZWQtcHJvZy1pZD4KPGFjdGl2aXR5LWlkPjIwOTk3MTwvYWN0aXZpdHktaWQ+CjxhY3Rpdml0eS1kZWYtaWQ+MjwvYWN0aXZpdHktZGVmLWlkPgo8d29ya291dC1pZD4xMDwvd29ya291dC1pZD4KPHdvcmtvdXQtc2Vzcy1pZD4xPC93b3Jrb3V0LXNlc3MtaWQ+Cjxsb2NhdGlvbi1pZD4zMTc8L2xvY2F0aW9uLWlkPgo8c291cmNlPjA8L3NvdXJjZT4KPC92aXZvLWJsb2NrPgo8L3dvcmtvdXQ+</presetFile></workoutSegments><workoutSegments><workoutId>2</workoutId><workoutName>Cascades</workoutName><equipment>Recumbent Bike</equipment><time>10.0</time><level>4.0</level><goal>4</goal><deviceType>12</deviceType><deviceType>30</deviceType><deviceType>32</deviceType><deviceType>31</deviceType><filename>10 min Cascades Recumbent Bike 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MTg6NTQrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOToxODo1NCswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjMyPC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4zMDwvcHJvZ3JhbS10eXBlPgo8Z29hbC10eXBlPjE8L2dvYWwtdHlwZT4KPGdvYWwtdmFsdWU+NjAwLjA8L2dvYWwtdmFsdWU+CjxsZXZlbD40LjA8L2xldmVsPgo8L3dvcmtvdXQtcHJlc2V0Pgo8dml2by1ibG9jaz4KPHVzZXItaWQ+NTE8L3VzZXItaWQ+Cjxhc3NpZ25lZC1wcm9nLWlkPjgzMTc1PC9hc3NpZ25lZC1wcm9nLWlkPgo8YWN0aXZpdHktaWQ+MjA5OTczPC9hY3Rpdml0eS1pZD4KPGFjdGl2aXR5LWRlZi1pZD4zPC9hY3Rpdml0eS1kZWYtaWQ+Cjx3b3Jrb3V0LWlkPjExPC93b3Jrb3V0LWlkPgo8d29ya291dC1zZXNzLWlkPjE8L3dvcmtvdXQtc2Vzcy1pZD4KPGxvY2F0aW9uLWlkPjMxNzwvbG9jYXRpb24taWQ+Cjxzb3VyY2U+MDwvc291cmNlPgo8L3Zpdm8tYmxvY2s+Cjwvd29ya291dD4=</presetFile></workoutSegments><workoutSegments><workoutId>5</workoutId><workoutName>Reverse</workoutName><equipment>Cross-Trainer</equipment><time>15.0</time><level>6.0</level><goal>4</goal><deviceType>5</deviceType><deviceType>6</deviceType><deviceType>16</deviceType><deviceType>17</deviceType><deviceType>24</deviceType><deviceType>26</deviceType><deviceType>25</deviceType><deviceType>2</deviceType><deviceType>43</deviceType><filename>Cross-Trainer 15 Min Reverse Workout 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MDk6MjArMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOTowOToyMCswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjI2PC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4xMzwvcHJvZ3JhbS10eXBlPgo8Z29hbC10eXBlPjE8L2dvYWwtdHlwZT4KPGdvYWwtdmFsdWU+OTAwLjA8L2dvYWwtdmFsdWU+CjxsZXZlbD42LjA8L2xldmVsPgo8L3dvcmtvdXQtcHJlc2V0Pgo8dml2by1ibG9jaz4KPHVzZXItaWQ+NTE8L3VzZXItaWQ+Cjxhc3NpZ25lZC1wcm9nLWlkPjgzMTY4PC9hc3NpZ25lZC1wcm9nLWlkPgo8YWN0aXZpdHktaWQ+MjA5OTY2PC9hY3Rpdml0eS1pZD4KPGFjdGl2aXR5LWRlZi1pZD40PC9hY3Rpdml0eS1kZWYtaWQ+Cjx3b3Jrb3V0LWlkPjE2PC93b3Jrb3V0LWlkPgo8d29ya291dC1zZXNzLWlkPjE8L3dvcmtvdXQtc2Vzcy1pZD4KPGxvY2F0aW9uLWlkPjMxNzwvbG9jYXRpb24taWQ+Cjxzb3VyY2U+MDwvc291cmNlPgo8L3Zpdm8tYmxvY2s+Cjwvd29ya291dD4=</presetFile></workoutSegments><workoutSegments><workoutId>7</workoutId><workoutName>Fit Test</workoutName><equipment>Cross-Trainer</equipment><level>1.0</level><deviceType>5</deviceType><deviceType>6</deviceType><deviceType>16</deviceType><deviceType>17</deviceType><deviceType>24</deviceType><deviceType>26</deviceType><deviceType>25</deviceType><deviceType>2</deviceType><deviceType>43</deviceType><filename>Fit Test Cross-Trainer 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MTA6MzIrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOToxMDozMiswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjI2PC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4yMDwvcHJvZ3JhbS10eXBlPgo8Z29hbC10eXBlPjA8L2dvYWwtdHlwZT4KPGxldmVsPjEuMDwvbGV2ZWw+Cjwvd29ya291dC1wcmVzZXQ+Cjx2aXZvLWJsb2NrPgo8dXNlci1pZD41MTwvdXNlci1pZD4KPGFzc2lnbmVkLXByb2ctaWQ+ODMxNjk8L2Fzc2lnbmVkLXByb2ctaWQ+CjxhY3Rpdml0eS1pZD4yMDk5Njc8L2FjdGl2aXR5LWlkPgo8YWN0aXZpdHktZGVmLWlkPjQ8L2FjdGl2aXR5LWRlZi1pZD4KPHdvcmtvdXQtaWQ+MzI8L3dvcmtvdXQtaWQ+Cjx3b3Jrb3V0LXNlc3MtaWQ+MTwvd29ya291dC1zZXNzLWlkPgo8bG9jYXRpb24taWQ+MzE3PC9sb2NhdGlvbi1pZD4KPHNvdXJjZT4wPC9zb3VyY2U+Cjwvdml2by1ibG9jaz4KPC93b3Jrb3V0Pg==</presetFile></workoutSegments><workoutSegments><workoutId>12</workoutId><workoutName>Interval</workoutName><equipment>Upright Bike</equipment><time>20.0</time><level>5.0</level><goal>4</goal><deviceType>11</deviceType><deviceType>27</deviceType><deviceType>29</deviceType><deviceType>28</deviceType><deviceType>0</deviceType><deviceType>42</deviceType><filename>Upright Bike Interval 01.xml</filename><presetFile>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHdvcmtvdXQ+Cjx2ZXJzaW9uPjIuMDwvdmVyc2lvbj4KPGNyZWF0ZS1kYXRlPjIwMTEtMDMtMjRUMTk6MDI6MjYrMDA6MDA8L2NyZWF0ZS1kYXRlPgo8bW9kaWZ5LWRhdGU+MjAxMS0wMy0yNFQxOTowMjoyNiswMDowMDwvbW9kaWZ5LWRhdGU+CjxkZXZpY2UtaW5mbz4KPGRldmljZS10eXBlPjI5PC9kZXZpY2UtdHlwZT4KPC9kZXZpY2UtaW5mbz4KPHVzZXItcHJvZmlsZT4KPHVzZXItaWQ+MDAwMkBzdHJvc3MuY29tPC91c2VyLWlkPgo8Z2VuZGVyPjE8L2dlbmRlcj4KPGFnZT41MjwvYWdlPgo8d2VpZ2h0IHVuaXRzPSIwIj4xODAuMDwvd2VpZ2h0Pgo8aGVpZ2h0IHVuaXRzPSIxIj5udWxsPC9oZWlnaHQ+Cjx1bml0cz4xPC91bml0cz4KPC91c2VyLXByb2ZpbGU+Cjx3b3Jrb3V0LXByZXNldD4KPHByb2dyYW0tdHlwZT4yOTwvcHJvZ3JhbS10eXBlPgo8Z29hbC10eXBlPjE8L2dvYWwtdHlwZT4KPGdvYWwtdmFsdWU+MTIwMC4wPC9nb2FsLXZhbHVlPgo8bGV2ZWw+NS4wPC9sZXZlbD4KPC93b3Jrb3V0LXByZXNldD4KPHZpdm8tYmxvY2s+Cjx1c2VyLWlkPjUxPC91c2VyLWlkPgo8YXNzaWduZWQtcHJvZy1pZD44MzE2MTwvYXNzaWduZWQtcHJvZy1pZD4KPGFjdGl2aXR5LWlkPjIwOTk1OTwvYWN0aXZpdHktaWQ+CjxhY3Rpdml0eS1kZWYtaWQ+MjwvYWN0aXZpdHktZGVmLWlkPgo8d29ya291dC1pZD4zMTwvd29ya291dC1pZD4KPHdvcmtvdXQtc2Vzcy1pZD4xPC93b3Jrb3V0LXNlc3MtaWQ+Cjxsb2NhdGlvbi1pZD4zMTc8L2xvY2F0aW9uLWlkPgo8c291cmNlPjA8L3NvdXJjZT4KPC92aXZvLWJsb2NrPgo8L3dvcmtvdXQ+</presetFile></workoutSegments></ns2:predefinedPresets>
}