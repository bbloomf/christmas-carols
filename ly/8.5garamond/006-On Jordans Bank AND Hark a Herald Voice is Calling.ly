\version "2.24.0"
\include "util.ly"
\header {
  tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 50))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 70))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #006
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCaps advent}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key bes \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  %\tempo 4 = 56
  \partial 4 f4 |
  bes f g g |
  f ees d d |
  ees d c f |
  
  f e f\fermata \bar"" f |
  bes c d bes |
  ees d c d |
  
  bes g f bes |
  bes a bes\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  f d ees ees8[ d] |
  c4 a bes bes |
  bes bes c d |
  
  d c c f |
  f ees d g8[ f] |
  ees4 f f f |
  
  f ees f d |
  g f f \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  On Jor -- dan’s bank the Bap -- tist’s cry
  An -- noun -- ces that the Lord is nigh;
  Come, then, and heark -- en, for he brings
  Glad tid -- ings from the King of kings!
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  Then cleansed be ev -- ’ry soul from sin;
  Make straight the way for God with -- in;
  Pre -- pare we in our hearts a home,
  Where such a might -- y Guest may come.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  For Thou art our sal -- va -- tion, Lord,
  Our Ref -- uge and our great re -- ward.
  With -- out Thy grace our souls must fade
  And with -- er like a flow’r de -- cayed.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  Stretch forth Thine hand, to heal our sore,
  And make us rise and fall no more;
  Once more up -- on Thy peo -- ple shine,
  And fill the world with love di -- vine.
}
altoWordsV = \lyricmode {
  \dropLyricsV
  \set stanza = #"5. "
  All praise, e -- ter -- nal Son, to Thee,
  Whose ad -- vent doth Thy peo -- ple free,
  Whom with the Fa -- ther, we a -- dore,
  And Ho -- ly Ghost, for ev -- er -- more.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  bes4 |
  bes bes bes ees, |
  f f f f |
  g f8[ g] a4 a |
  
  bes8[ a] g4 a a |
  bes g8[ a] bes4 bes |
  bes8[ a] bes4 a bes |
  
  bes bes bes bes |
  c c d \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  bes4 |
  d bes ees c8[ bes] |
  a4 f bes bes |
  g bes f d' |
  
  bes c f, f'8[ ees] |
  d4 c bes ees8[ d] |
  c4 d8[ ees] f4 bes, |
  
  d ees d g |
  ees f bes, \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"On Jordan’s Bank"}}
    poet = \markup\concat{\italic"Jordanis oras prævia" \oldStyleNum", by Charles Coffin (1676–1749)"}
    meter = \markup\oldStyleNum"Translated by John Chandler (1806–1876)"
    composer = \markup\italic"Winchester New"
    arranger = \markup\concat{"Adapted from Chorale in " \italic"Musikalisches Hand-Buch" \oldStyleNum", 1690"}
    tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
  }
}







global = {
  \key e \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  %\tempo 4 = 66
  e4 gis b b |
  a cis cis b \bar "||"
  gis ais b b |
  cis cis b2 \bar "||"
  
  b4 a gis b |
  a gis fis e \bar "||"
  fis gis a gis |
  fis fis e2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  b4 b b dis |
  e e e e |
  e e dis fis |
  gis fis8[ e] dis2 |
  
  dis4 e8[ fis] e4 fis |
  e e dis e |
  cis eis fis e |
  e dis e2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Hark! a her -- ald voice is call -- ing:
  ‘Christ is nigh,’ it seems to say;
  \set associatedVoice = "altos"
  ‘Cast a -- way the dreams of dark -- ness,
  \unset associatedVoice
  O ye chil -- dren of the day!’
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  Star -- tled at the sol -- emn warn -- ing,
  Let the earth -- bound soul a -- rise;
  \set associatedVoice = "altos"
  Christ, her Sun, all sloth dis -- pell -- ing,
  \unset associatedVoice
  Shines up -- on the morn -- ing skies.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Lo! the Lamb, so long ex -- pec -- ted,
  Comes with par -- don down from heav’n;
  \set associatedVoice = "altos"
  Let us haste, with tears of sor -- row,
  \unset associatedVoice
  One and all to be for -- giv’n;
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  So when next He comes with glo -- ry,
  Wrap -- ping all the earth in fear,
  \set associatedVoice = "altos"
  May He then as our de -- fend -- er
  \unset associatedVoice
  On the clouds of heav’n ap -- pear.
}
altoWordsV = \lyricmode {
  \dropLyricsV
  \set stanza = #"5. "
  Hon -- or, glo -- ry, vir -- tue, mer -- it,
  To the Fa -- ther and the Son,
  \set associatedVoice = "altos"
  With the co -- e -- ter -- nal Spi -- rit,
  \unset associatedVoice
  While un -- end -- ing a -- ges run.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  gis4 b fis fis |
  e a a gis |
  b e, fis b |
  b ais b2 |
  
  dis4 cis8[ b] b4 b |
  cis b b8[ a] gis4 |
  a b cis b |
  cis b8[ a] gis2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 e dis b |
  cis a8[ b] cis[ dis] e4 |
  e cis b dis |
  e fis b,2 |
  
  b4 cis8[ dis] e4 dis |
  cis e b cis |
  a gis fis gis |
  a b <e e,>2 \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Hark! a Herald Voice is Calling"}}
    poet = \markup\concat{\italic"Vox clara ecce intonat" \oldStyleNum", 6th Century"}
    meter = \markup\oldStyleNum"Translated by Edward Caswall (1814–1878)"
    composer = \markup\oldStyleNum"William Henry Monk (1823–1889)"
    tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
  }
}

