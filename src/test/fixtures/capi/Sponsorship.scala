package com.gu.contentapi.client.model.v1

case class Sponsorship(sponsorshipType: SponsorshipType, sponsorName: String, sponsorLogo: String, sponsorLink: String, targeting: Option[SponsorshipTargeting] = None, aboutLink: Option[String] = None, sponsorLogoDimensions: Option[SponsorshipLogoDimensions] = None, highContrastSponsorLogo: Option[String] = None, highContrastSponsorLogoDimensions: Option[SponsorshipLogoDimensions] = None, validFrom: Option[CapiDateTime] = None, validTo: Option[CapiDateTime] = None) extends TStruct

object Sponsorship extends TStructCodec[Sponsorship] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  val w3 = Witness(3)

  val w4 = Witness(4)

  val w5 = Witness(5)

  val w6 = Witness(6)

  val w7 = Witness(7)

  val w8 = Witness(8)

  val w9 = Witness(9)

  val w10 = Witness(1)

  val w11 = Witness(11)

  implicit val r1 = new TFieldCodec[w1.T, SponsorshipType]()

  implicit val r2 = new TFieldCodec[w2.T, String]()

  implicit val r3 = new TFieldCodec[w3.T, String]()

  implicit val r4 = new TFieldCodec[w4.T, String]()

  implicit val r5 = new TFieldCodec[w5.T, Option[SponsorshipTargeting]]()

  implicit val r6 = new TFieldCodec[w6.T, Option[String]]()

  implicit val r7 = new TFieldCodec[w7.T, Option[SponsorshipLogoDimensions]]()

  implicit val r8 = new TFieldCodec[w8.T, Option[String]]()

  implicit val r9 = new TFieldCodec[w9.T, Option[SponsorshipLogoDimensions]]()

  implicit val r10 = new TFieldCodec[w10.T, Option[CapiDateTime]]()

  implicit val r11 = new TFieldCodec[w11.T, Option[CapiDateTime]]()

  override val defaults = HMap[TFieldCodec](w5.value -> None, w6.value -> None, w7.value -> None, w8.value -> None, w9.value -> None, w10.value -> None, w11.value -> None)

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.sponsorshipType, w2.value -> x.sponsorName, w3.value -> x.sponsorLogo, w4.value -> x.sponsorLink, w5.value -> x.targeting, w6.value -> x.aboutLink, w7.value -> x.sponsorLogoDimensions, w8.value -> x.highContrastSponsorLogo, w9.value -> x.highContrastSponsorLogoDimensions, w10.value -> x.validFrom, w11.value -> x.validTo)

  override def decode = (m) ⇒ for {
    sponsorshipType ← m.get(w1.value).orElse(defaults.get(w1.value))

    sponsorName ← m.get(w2.value).orElse(defaults.get(w2.value))

    sponsorLogo ← m.get(w3.value).orElse(defaults.get(w3.value))

    sponsorLink ← m.get(w4.value).orElse(defaults.get(w4.value))

    targeting ← m.get(w5.value).orElse(defaults.get(w5.value))

    aboutLink ← m.get(w6.value).orElse(defaults.get(w6.value))

    sponsorLogoDimensions ← m.get(w7.value).orElse(defaults.get(w7.value))

    highContrastSponsorLogo ← m.get(w8.value).orElse(defaults.get(w8.value))

    highContrastSponsorLogoDimensions ← m.get(w9.value).orElse(defaults.get(w9.value))

    validFrom ← m.get(w10.value).orElse(defaults.get(w10.value))

    validTo ← m.get(w11.value).orElse(defaults.get(w11.value))
  } yield Sponsorship(sponsorshipType, sponsorName, sponsorLogo, sponsorLink, targeting, aboutLink, sponsorLogoDimensions, highContrastSponsorLogo, highContrastSponsorLogoDimensions, validFrom, validTo)
}