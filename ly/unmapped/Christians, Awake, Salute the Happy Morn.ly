\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Christians, Awake, Salute the Happy Morn"}}
  poet = \markup\oldStyleNum"John Byrom (1692–1763)"
  composer = \markup\oldStyleNum"John Wainwright (1723–1768)"
  tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -1)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #060
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key c \major
  \time 4/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0 . 0)
}

sopMusic = \relative c' {
  \repeat volta 3 {
    c2 c4 d |
    e2. f4 |
    g e f g |
    a1 \bar "||"
    g2 a4 b |
    
    c d e d |
    c2 b |
    c1 \bar "||"
    c2 e4 g, |
    a2. g4 |
    a b c d |
    b1 \bar "||"
    
    b2 c4 b |
    a gis a b |
    c2 b |
    a1 \bar "||"
    a2 g4 f |
    e2 c |
    
    f4 e d c |
    g'2. \bar"||" g4 |
    a2 b |
    c4 d e d |
    c2 b |
    c1 \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c2 c4 b |
  c2. b4 |
  c c b c |
  c1 |
  c2 c4 f |
  
  g f e f |
  e2 d |
  e1 |
  e2 e4 g |
  g( f8[ e] f4) c4 |
  f f g f |
  d1 |
  
  e2 e4 gis |
  a e c f |
  e2. d4 |
  c1 |
  c2 d4 b |
  c2 c |
  
  b4 c b a |
  b2. c4 |
  a'( g) f2 |
  g4 g g f |
  e2 d4( e8[ f]) |
  e1
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  \set associatedVoice = "altos"
  Christ -- ians, a -- wake, sa -- lute the hap -- py morn,
  Where -- on the Sav -- ior of man -- kind was born;
  Rise to a -- dore __ the mys -- ter -- y of love,
  \unset associatedVoice
  Which hosts of an -- gels chant -- ed from a -- bove;
  With them the joy -- ful ti -- dings first be -- gun
  Of God In -- car -- nate and the Vir -- gin’s Son.
  
  \set stanza = #"4. "
  \set associatedVoice = "altos"
  To Beth -- l’hem straight the hap -- py shep -- herds ran,
  To see the won -- der God had wrought for man:
  And found, with Jo -- seph and the bless -- ed maid,
  \unset associatedVoice
  Her Son, the Sav -- ior in a man -- ger laid;
  A -- mazed the won -- drous sto -- ry they pro -- claim,
  The ear -- liest her -- alds of the Sav -- ior’s name.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"2. "
  \set associatedVoice = "altos"
  Then to the watch -- ful shep -- herds it was told,
  Who heard th’an -- gel -- ic her -- ald’s voice: “Be -- hold,
  I bring good ti -- dings of a Sav -- ior’s birth
  \unset associatedVoice
  To you and all the na -- tions up -- on earth:
  This day hath God ful -- fill’d His prom -- ised word,
  This day is born a Sav -- ior, Christ, the Lord.”
  
  \set stanza = #"5. "
  \set associatedVoice = "altos"
  Let us, like these good shep -- herds, then em -- ploy
  Our grate -- ful voi -- ces to pro -- claim the joy;
  Trace we the Babe, __ Who hath re -- trieved our loss,
  \unset associatedVoice
  From His poor man -- ger to His bit -- ter Cross;
  Tread -- ing His steps, as -- sist -- ed by His grace,
  Till man’s first heav’n -- ly state a -- gain takes place.
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  \set associatedVoice = "altos"
  He spake; and straight -- way the ce -- les -- tial choir
  In hymns of joy, un -- known be -- fore, con -- spire:
  The prais -- es of __ re -- deem -- ing love they sang,
  \unset associatedVoice
  And heav’n’s whole arch with al -- le -- lu -- ias rang:
  God’s high -- est glo -- ry was their an -- them still,
  Peace up -- on earth, and un -- to men, good -- will.
  
  \set stanza = #"6. "
  \set associatedVoice = "altos"
  Then may we hope, th’an -- gel -- ic thrones a -- mong,
  To sing, re -- deemed, a glad tri -- um -- phal song;
  He, that was borne __ up -- on this joy -- ful day,
  \unset associatedVoice
  A -- round us all His glo -- ry shall dis -- play;
  Saved by His love, in -- ces -- sant we shall sing
  Of an -- gels and of an -- gel -- men, the King.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}

tenorMusic = \relative c {
  e2 e4 g |
  g2. g4 |
  g g g g |
  f1 |
  g2 f4 d' |
  
  c b c a |
  g2 g |
  g1 |
  g2 c4 c |
  c2. c4 |
  c d c a |
  g1 |
  
  gis2 a4 d |
  e b a a |
  a2 gis |
  a1 |
  e2 g4 g |
  g2 g |
  
  g4 g g fis |
  g2. c4 |
  c2 d |
  c4 b c a |
  g2 g |
  g1
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c2 c4 g |
  c2. d4 |
  e c d e |
  f1 |
  e2 f4 d |
  e d c f |
  g2 g, |
  c1 |
  c2 c4 e |
  f2. e4 |
  f d e f |
  g1 |
  
  e2 a,4 b |
  c e f d |
  e2 e |
  a,1 |
  a2 b4 g |
  c2 e |
  
  d4 c b a |
  g2. e'4 |
  f( e) d2 |
  e4 g c f, |
  g2 g |
  c,1
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global {\once \override Score.RehearsalMark #'self-alignment-X = #LEFT \mark "Majestically" \sopMusic \sopMusic} >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 2 \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" 
      \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "basses" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" 
      \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "basses" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" 
      \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "basses" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" 
      \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "basses" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" 
      \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "basses" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" 
      \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "basses" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
%      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
%      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 4 = 135
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
