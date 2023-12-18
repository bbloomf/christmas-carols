\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #136
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
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
  \time 2/4
  \autoBeamOff
}

sopMusic = \relative c' {
bes'4 a8 g |
f4. f8 |
g4 a |
bes2 |
bes4 a8  g |
f4. f8 |

g4 a |
bes4. bes8 |
a4. a8 |
g2 |
a4. a8 |
g2 |

bes4 c |
d2 |
bes4 c |
d2 | \break
d4 c8 bes |
a4 g |

a4 fis |
g2 |
d'4 c8 bes |
a4 g |
a fis |
g2 \bar "|."
}

altoMusic = \relative c' {
bes'4 a8 g |
f4. f8 |
ees4 ees |
d2 |
g4 f8 ees |
d4. d8 |

ees8[ d] c[ ees] |
d4. d8 |
d4. c8 |
bes2 |
d4. c8 |
bes2 |

g'4 a |
f2 |
g4 a |
f2 |
f4 ees8 d |
c4 bes8[ ees] |

ees4 d8[ c] |
bes2 |
d4 ees8 d |
c4 bes8[ ees] |
ees4 d8[ c] |
bes2 \bar "|."
}
altoWords = {
  \dropLyricsIX
  \lyricmode {
    \set stanza = #"1. "
    While by the sheep we watched at night,
    Glad tid -- ings brought an an -- gel bright.
  }
  \set stanza = \markup\dynamic"f  "
  \lyricmode {
    How great our joy!
  }
  \set stanza = \markup\dynamic"p "
  \lyricmode {
    Great our joy!
  }
  \set stanza = \markup\dynamic"f "
  \lyricmode {
    Joy, joy, joy!
  }
  \set stanza = \markup\dynamic"p "
  \lyricmode {
    Joy, joy, joy!
  }
  \set stanza = \markup\dynamic"f  "
  \lyricmode {
    Praise we the Lord in heav’n on high!
  }
  \set stanza = \markup\dynamic"  p "
  \lyricmode {
    Praise we the Lord in heav’n on high!
  }
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
There shall be born, so he did say,
In Beth -- le -- hem a Child to -- day.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
There shall the Child lie in a stall,
This Child who shall re -- deem us all.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
This gift of God we’ll cher -- ish well,
That ev -- er joy our hearts shall fill.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
bes4 a8 g |
f4. bes8 |
bes4 f |
bes2 |
d4 d8 bes |
bes4. bes8 |

bes4 f |
\partial 4. f4. |\break
\partial 8 g8 |
fis4. fis8 |
g2 |
fis4. fis8 |
g2 |

d'4 f |
d2 |
d4 f |
d2 |
bes4 f8 f |
fis4 g |

c a |
g2 |
f4 fis8 g |
fis4 g |
c a |
g2 \bar "|."

}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
bes'4 a8 g |
f4. d8 |
ees[ d] c4 |
bes2 |
g'4 d8 ees |
bes4. bes8 |

ees4 f |
bes,4. g8 |
d'4. d8 |
g,2 |
d'4. d8 |
g,2 |

g'4 f |
bes2 |
g4 f |
bes2 |
bes,4 a8 bes |
d4 ees |

c d |
g,2 |
bes4 a8 g |
d'4 ees |
c d |
g,2 \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"How Great Our Joy!"}}
    poet = \markup\oldStyleNum"German Carol"
    meter = \markup\oldStyleNum"Translated by Theodore Baker (1851–1934)"
    composer = \markup\oldStyleNum"German Melody"
    arranger = \markup\oldStyleNum"Arranged by Hugo Jüngst (1853–1923)"
    tagline = \markup { "from" \italic "CyberHymnal.org"}
  }
}
\markup\fill-line{\concat{"from " \italic "CyberHymnal.org"}}
\markup\vspace#2.8

















%IF_NOT_LESSER
global = {
  \key a \major
  \time 4/4
  \autoBeamOff
}
globalNoTime = {
  \key a \major
  \autoBeamOff
}
sopMusic = \relative c' {
  \once \override Score.RehearsalMark.self-alignment-X = #LEFT
  \mark \markup\italic "Con spirito."
  \partial 2 e4 e |
  fis e a b |
  cis8[ d] e4 cis b |
  a gis8[ a] b4 fis |
  
  gis4( fis) e e |
  fis e a b |
  cis8[ d] e4 a, b |
  cis b8[ a] b4 gis |
  \partial 2 a2 \bar "||" \break
}
sopMusicII = \relative c' {
  \partial 2 b'2\rest |
  e,4 a a gis |
  a cis fis, gis |
  a cis b gis |
  
  a(^\< fis) e^\f d' |
  d cis b fis' |
  fis e a, b |
  cis a a gis |
  \partial 2. a2.\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  cis4 cis |
  d e cis e |
  e a gis gis |
  fis e e dis |
  
  e( d) cis b |
  cis cis cis e |
  e g fis fis8[ gis] |
  a4 fis fis e8[ d] |
  cis2 \bar "||"
}
altoMusicII = \relative c' {
  s2 |
  cis4 e e e |
  e a8[ gis] fis4 fis |
  fis a eis eis |
  
  cis( d) b e |
  e e fis gis |
  gis a a fis |
  eis cis fis e8[ d] |
  cis2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Why, Most High -- est, art Thou ly -- ing, In a man -- ger poor and low?
  Thou, the fires of heav’n sup -- ply -- ing, Come a sta -- ble’s cold to know?
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  On a Moth -- er’s breast Thou sleep -- est,
  Moth -- er, yet a Vir -- gin still;
  Sad, with eyes be -- dimmed Thou weep -- est,
  Eyes, which Heav’n with glad -- ness fill.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Weak the Strong, of strength the Giv -- er:
  Small, Whose arms cre -- a -- tion span;
  Bound, Who on -- ly can de -- liv -- er;
  Born is He Who ne’er be -- gan.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a4 a |
  a a a gis |
  a8[ b] cis4 e d |
  cis cis gis8[ a] b4 |
  
  b2 cis4 gis |
  a gis a gis |
  a8[ b] cis4 d b |
  e d8[ cis] d4 b |
  a2 \bar "||"
}
tenorMusicII = \relative c {
  r2 |
  e4 cis' b b |
  a e' d d |
  cis cis cis b |
  
  a2 d4 gis, |
  a a8[ gis] fis4 b |
  b cis cis d |
  gis, a b b |
  a2.\fermata \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  a4 a |
  d cis fis e |
  a a e e |
  fis cis b b |
  
  e( fis8[ gis]) a4 gis |
  fis cis fis e8[ d] |
  cis4 a d d |
  cis fis d e |
  a,2 \bar "||"
}
bassMusicII = \relative c {
  e4 b' |
  b a e d' |
  d cis cis b |
  a gis8[ fis] gis4 cis, |
  
  fis2 gis4 e |
  s a, d d |
  d cis s d |
  cis s d s |
  e2.\fermata \bar "|."
}
bassIIMusic = \relative c {
  g2\rest |
  a4. cis8 e4 e |
  a, a b b |
  cis cis cis cis |
  
  fis2 gis4 e |
  a s s s |
  s s fis s |
  s fis s e |
  a,2. \bar "|."
}
bassWords = \lyricmode {}
dropLyrics = {
  \override LyricText.extra-offset = #'(0 . -1.3)
  \override LyricHyphen.extra-offset = #'(0 . -1.3)
  \override LyricExtender.extra-offset = #'(0 . -1.3)
  \override StanzaNumber.extra-offset = #'(0 . -1.3)
}
bassWordsChorus = {
  \dropLyrics
  \set stanza = \markup\dynamic"f "
  \lyricmode {
    O what works of love stu -- pen -- dous,
    Je -- su,
    \set associatedVoice = "tenorsII"
    Were sal -- va -- tion’s price!
    Burn -- ing wert Thou to be -- friend us,
    Ex -- iles far from Pa -- ra -- dise.
  }
}
chorusWordsBass = {
  \lyricmode {
    O what works of love stu -- pen -- dous
    Were sal -- va -- tion’s price!
    Burn -- ing \set associatedVoice = "bassI" wert Thou to be -- friend \unset associatedVoice us,
    \set associatedVoice = "bassI" 
    Ex -- iles \unset associatedVoice far \set associatedVoice = "bassI" from \unset associatedVoice Pa -- ra -- dise.
  }
}
chorusWords = {
  \set stanza = \markup\dynamic"p"
  \lyricmode {
    O what works of love stu -- pen -- dous
    Were sal -- va -- tion’s price!
    Burn -- ing wert Thou to be -- friend us,
    Ex -- iles far from Pa -- ra -- dise.
  }
}
pianoRH = \relative c' {
  s1*8 |
  r2 |
  <cis e>4 <cis e a> <b e a> <d e gis> |
  << \new Voice { \voiceOne <d e a> <cis e cis'> <cis d fis> <b d fis gis> }
     \new Voice { \voiceTwo s4 a'8[ gis] s2 }
  >>
  <a fis cis>4 <cis a cis,> <b eis, cis> <gis eis b> |
  
  <a cis,> <fis d> <e d> <e d'> |
  <e d'> <e cis'> <fis b> <gis fis'> |
  <gis fis'> <a e'> a <fis b> |
  <eis cis'> <cis a'> <fis a> << \new Voice {\voiceOne gis} \new Voice {\voiceTwo e8[ d] } >> |
  <cis a'>2. \bar "|."
}
pianoLH = \relative c' {
  s1*8 |
  \partial 2 e,4 b' |
  << \new Voice { \voiceOne <b e,>4 a e <e b'> }
     \new Voice { \voiceTwo a,4~ a8[ cis] } >> |
  a4 a b b |
  <cis a'> << \new Voice {\voiceTwo cis} \new Voice {\voiceOne gis'8[ fis]}>> <gis cis,>4 cis, |
  
  <fis a>2 <gis b>4 <gis e> |
  a << \new Voice { \voiceOne a8[ gis] } \new Voice {\voiceTwo a,4} >> <d fis>4 <d b'> |
  <d b'> <cis cis'> <fis cis'> <d d'> |
  <cis gis'> <fis a> <d b'> <e b'> |
  <a e a,>2. \bar "|."
}

\score {
  <<
   \new ChoirStaff <<
    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
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
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
  >>
   \new ChoirStaff <<
    \new Staff = womenII <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \new Voice = "sopranosII" { \globalNoTime s1*8 \voiceOne \sopMusicII }
      \new Voice = "altosII" { \globalNoTime s1*8 \voiceTwo \altoMusicII }
    >>
    \new Lyrics \with { alignBelowContext = #"womenII" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranosII" \chorusWords
%    \new Staff = altosII <<
%      \new Voice = "altosII" { \globalNoTime s1*8 \altoMusicII }
%    >>
%    \new Lyrics \with { alignBelowContext = #"altosII" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altosII" \chorusWords
    \new Staff = tenorsII <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \clef "G_8"
      \new Voice = "tenorsII" { \globalNoTime s1*8 \tenorMusicII }
    >>
%    \new Lyrics \with { alignBelowContext = #"tenorsII" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenorsII" \chorusWords
    \new Staff = bassesII <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \clef bass
      \new Voice = "bassI" { \globalNoTime \voiceOne s1*8 << \bassMusicII >> }
      \new Voice = "bassII" { \globalNoTime \voiceTwo s1*8 << \bassIIMusic >> }
    >>
    \new Lyrics \with { alignBelowContext = #"tenorsII" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "bassI" \bassWordsChorus
    \new Lyrics \with { alignBelowContext = #"bassesII" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "bassII" \chorusWordsBass
    
   >>
   \new PianoStaff <<
      \new Staff {
        \new Voice { \global \pianoRH }
      }
      \new Staff {
        \clef "bass" \global \pianoLH
      }
   >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }

%6.14 \context {\Lyrics\override LyricText.font-size = #0.8 }
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves
      \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Jesus in the Manger"}}
    poet = \markup\oldStyleNum"Translated by Rev. H.R. Bramley (1833–1917) from Latin"
    composer = \markup\oldStyleNum"Henry Smart (1813–1879)"
    tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
  }
}
\header {
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
%END_IF_NOT_LESSER

