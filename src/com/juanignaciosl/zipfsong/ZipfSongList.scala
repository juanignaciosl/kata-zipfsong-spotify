package com.juanignaciosl.zipfsong

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * @author juanignaciosl
 * @see https://www.spotify.com/es/jobs/tech/zipfsong/
 *
 */
class ZipSongListTest extends FlatSpec with ShouldMatchers {

  "Quality of position 1, 10 listenings" should "be 10" in {
    ZipfSongMeter.qualityOf(1, 10) should be(10)
  }

  "Quality of position 2, 10 listenings" should "be 20" in {
    ZipfSongMeter.qualityOf(2, 10) should be(20)
  }

  "Quality of (4, 25)" should "be bigger than (2, 30)" in {
    ZipfSongMeter.qualityOf(4, 20) should be > ZipfSongMeter.qualityOf(2, 30)
  }

  "Quality of (2, 30)" should "be bigger than (1, 30)" in {
    ZipfSongMeter.qualityOf(2, 30) should be > ZipfSongMeter.qualityOf(1, 30)
  }

  "Quality of (2, 30)" should "be bigger than (3, 15)" in {
    ZipfSongMeter.qualityOf(2, 30) should be > ZipfSongMeter.qualityOf(3, 15)
  }

  "One song" should "be that song" in {
    val songList = (30, "one") :: Nil
    val zipfSongList = new ZipfSongList(songList)
    zipfSongList.sort(1) should be("one" :: Nil)
  }

  "A 2 songs Zipf list of ((30, \"one\"), (30, \"two\"), (15, \"three\"), (25, \"four\"))" should "be (\"four\", \"two\")" in {
    val songList = (30, "one") :: (30, "two") :: (15, "three") :: (25, "four") :: Nil;
    val zipfSongList = new ZipfSongList(songList)
    zipfSongList.sort(2) should be("four" :: "two" :: Nil)
  }

  "Second Spotify case" should "be (\"19_2000\", \"clint_eastwood\", \"tomorrow_comes_today\")" in {
    val songList = (197812, "re_hash") :: (78906, "5_4") :: (189518, "tomorrow_comes_today") :: (39453, "new_genious") :: (210492, "clint_eastwood") :: (26302, "man_research") :: (22544, "punk") :: (19727, "sound_check") :: (17535, "double_bass") :: (18782, "rock_the_house") :: (198189, "19_2000") :: (13151, "latin_simone") :: (12139, "starshine") :: (11272, "slow_country") :: (10521, "m1_a1") :: Nil;
    val zipfSongList = new ZipfSongList(songList)
    zipfSongList.sort(3) should be("19_2000" :: "clint_eastwood" :: "tomorrow_comes_today" :: Nil)
  }
}

object ZipfSongMeter {
  def qualityOf(positionOneBased: Int, listenings: Int) = {
    listenings * positionOneBased
  }
}

class ZipfSongList(val songList: List[(Int, String)]) {

  def sort(n: Int): List[String] = {
    songList.sortBy(x => (-ZipfSongMeter.qualityOf(songList.indexOf(x) + 1, x._1))).slice(0, n) map (_._2)
  }

}