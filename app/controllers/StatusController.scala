package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import org.joda.time._

import views._
import models.StatusDao
import mocks.VirtualTrainerMocks._
import mocks.VirtualTrainerMocks.FakeVirtualTrainer._
import scalaz._
import Scalaz._

object StatusController extends Controller with StatusDao {
  /**
   * Return the application status to external monitors. If the return string contains "ok"
   * then all is well in the application. Add any tests or checks you want in the model or below
   * in the controller
   *
   * import play.api.Play.current
   * play.api.Play.configuration.keys.toString gets all app settings
   * play.api.Play.configuration.getString("mrj.build").getOrElse("")
   *
   * @return
   */
  def status = Action {
    val s: String = "Version: " + current.configuration.getString("appVersion").
      getOrElse("") + "\n" + (if (checkAppStatus) "**** ok ****\n" else "**** fail ****\n")
    play.api.Logger.warn(s)
    Ok(html.status(s))
  }

  def vtTimingTest(num: Int) = Action {

    val validate = 0
    val register = 1
    val link = 2
    val login = 3
    val logout = 4

    val nums = scala.collection.mutable.ArraySeq(0, 0, 0, 0, 0)
    val durs = scala.collection.mutable.ArraySeq(0L, 0L, 0L, 0L, 0L)
    val errs = scala.collection.mutable.ArraySeq(0, 0, 0, 0, 0)
    val mins = scala.collection.mutable.ArraySeq(0L, 0L, 0L, 0L, 0L)
    val maxs = scala.collection.mutable.ArraySeq(0L, 0L, 0L, 0L, 0L)

    val srvt = new SemiRealVirtualTrainer()

    def setTimes(i: Int, d: Long) {
      durs(i) += d
      mins(i) = (d < mins(i) || mins(i) == 0L) ? d | mins(i)
      maxs(i) = (d > maxs(i)) ? d | maxs(i)
    }

    val start = DateTime.now
    for (i <- (0 until num)) {

      val vData = VtVarData()
      val regBody = srvt.vtRegisterBody(vtrRegParams(vData)).toOption.getOrElse("body not created")

      nums(validate) += 1
      var start = DateTime.now.getMillis
      val valResult = srvt.vtDoValidate(regBody)
      setTimes(validate, DateTime.now.getMillis - start + 1L)

      valResult.status match {
        case s if (s != 200) => errs(validate) += 1
        case _ =>

          nums(register) += 1
          start = DateTime.now.getMillis
          val regResult = srvt.vtDoRegister(regBody)
          setTimes(register, DateTime.now.getMillis - start + 1L)

          regResult.status match {
            case s if (s != 200) => errs(register) += 1
            case _ =>

              val vtUid = regResult.xml.map {
                x => (x \\ "userId").head.text
              }.getOrElse("0")
              nums(link) += 1
              start = DateTime.now.getMillis
              val linkResult = srvt.vtDoLink(vData.id, vtUid)
              setTimes(link, DateTime.now.getMillis - start + 1L)

              linkResult.status match {
                case s if (s != 200) => errs(link) += 1
                case _ =>

                  val loginBody = srvt.vtLoginBody(vData.id, vData.pwd)
                  nums(login) += 1
                  start = DateTime.now.getMillis
                  val loginResult = srvt.vtDoLogin(loginBody)
                  setTimes(login, DateTime.now.getMillis - start + 1L)

                  loginResult.status match {
                    case s if (s != 200) => errs(login) += 1
                    case _ =>

                      val tEx = """(.*oauth_token=\")([^\"]*).*""".r
                      val tsEx = """(.*oauth_token_secret=\")([^\"]*).*""".r

                      val hdr = loginResult.header("Authorization")
                      val tEx(_, vtToken) = tEx.findFirstIn(hdr.getOrElse("")).getOrElse("")
                      val tsEx(_, vtSecret) = tsEx.findFirstIn(hdr.getOrElse("")).getOrElse("")

                      nums(logout) += 1
                      start = DateTime.now.getMillis
                      val logoutResult = srvt.vtDoLogout(vtToken, vtSecret)
                      setTimes(logout, DateTime.now.getMillis - start + 1L)

                      logoutResult.status match {
                        case s if (s != 200) => errs(logout) += 1
                        case _ =>
                      }
                  }
              }
          }
      }
    }

    val avgs = Array(
      (nums(validate) > 0) ? (durs(validate).toDouble / nums(validate).toDouble) | 0d,
      (nums(register) > 0) ? (durs(register).toDouble / nums(register).toDouble) | 0d,
      (nums(link) > 0) ? (durs(link).toDouble / nums(link).toDouble) | 0d,
      (nums(login) > 0) ? (durs(login).toDouble / nums(login).toDouble) | 0d,
      (nums(logout) > 0) ? (durs(logout).toDouble / nums(logout).toDouble) | 0d
    )

    val result = new StringBuffer

    def writeResult(i: Int, s: String) {
      result.append("%s: %d calls, %f seconds avg duration, %f min, %f max, %d calls failed\n".
        format(s, nums(i), avgs(i) / 1000d, mins(i) / 1000d, maxs(i) / 1000d, errs(i)))
    }

    result.append("Tests started at %s\n".format(start))
    writeResult(validate, "Validate")
    writeResult(register, "Register")
    writeResult(link, "Link")
    writeResult(login, "Login")
    writeResult(logout, "Logout")
    result.append("Tests ended at %s\n".format(org.joda.time.DateTime.now))
    Ok(result.toString)
  }
}
