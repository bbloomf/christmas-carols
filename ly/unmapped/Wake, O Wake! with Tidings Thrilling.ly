\version "2.14.2"
\include "util.ly"
\version "2.14.2"
#(set-global-staff-size 14.9) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.9 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Wake, O Wake! with Tidings Thrilling"}}
  poet = \markup\concat{\italic"Wachet Auf!" \oldStyleNum" by P. Nicolai (1556–1608)"}
  meter = \markup\oldStyleNum"Translated by F.C.B."
  composer = \markup\oldStyleNum"P. Nicolai (1556–1608)"
  arranger = \markup\oldStyleNum"Adapted and arranged by J.S. Bach (1685–1750)"
  tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #007
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCaps advent}
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
global = {
  \key d \major
  \time 4/4
  \autoBeamOff
  \mergeDifferentlyHeadedOn
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo \markup\italic"Very slow and solemn" 4 = 64
  \repeat volta 2 {
    d4 fis a a |
    a a b2 |
    a2.\fermata \bar"||"
    
    a4 |
    d4 a d8[ e] fis4 |
    e d cis4( b) |
    a2.\fermata \bar"||"
    
    a4 |
    d a b fis |
    g8[ fis] e4 d\fermata \break
  }
  a'4 |
  a4. g8 fis4 e |
  
  d2.\fermata \bar "||"
  a'4 |
  a4. g8 fis4 e |
  d2\fermata \bar "||"
  
  e4 fis |
  g4 fis2\fermata \bar "||"
  a4 |
  b cis d d8[ e] |
  
  fis4 e d\fermata \bar"||"
  a4 |
  d a b fis |
  g8[ fis] e4 d2\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat volta 2 {
    a4 d fis e |
    fis e fis( e) |
    e2. |
    
    e4 |
    a 4. g8 fis[ g] a4 |
    a b b8( a4 gis8) |
    e2. |
    
    fis4 |
    g fis d d |
    d cis a
  }
  e'4 |
  fis4. e8 a,[ b] cis4 |
  
  b2. |
  cis8[ d] |
  e4 e e8( d4) cis8 |
  a2 |
  
  cis4 d |
  d8[ cis] d2 |
  fis4 |
  fis8[ e] e[ fis] fis4 fis8[ g] |
  
  a4. g8 fis4 |
  fis8[ g] |
  a[ g] a[ fis] g4 d |
  d cis a2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVI
  \set stanza = #"1. "
  Wake, o wake! with tid -- ings thrill -- ing
  The watch -- men all the air are fill -- ing,
  \set associatedVoice = "sopranos"
  A -- rise, Je -- ru -- sa -- lem, a -- rise!
  
  \set associatedVoice = "altos"
  The Bride -- groom comes in \set associatedVoice = "basses" sight,
  Raise high your tor -- ches \set associatedVoice = "tenors" bright!
  Al -- le -- lu -- ia!
  The wed -- ding song
  Swells loud and strong:
  \set associatedVoice = "sopranos"
  Go forth and join the fest -- al throng.
}
altoWordsII = \lyricmode {
  \dropLyricsVI
%\markup\italic
  Mid -- night strikes! no more de -- lay -- ing,
  ‘The hour has come!’ we hear them say -- ing.
  \set associatedVoice = "sopranos"
  Where are ye all, ye vir -- gins wise?
}
altoWordsIII = \lyricmode {
  \dropLyricsVI
  \set stanza = #"2. "
  Zi -- on hears the watch -- men shout -- ing,
  Her heart leaps up with joy un -- doubt -- ing,
  \set associatedVoice = "sopranos"
  She stands and waits with ea -- ger eyes;
  
  \set associatedVoice = "altos"
  Now come, Thou pre -- cious \set associatedVoice = "basses" Crown,
  Lord Je -- su, God’s own \set associatedVoice = "tenors" Son!
  Ho -- san -- _ na!
  Let us pre -- pare
  To fol -- low there,
  \set associatedVoice = "sopranos"
  Where in Thy sup -- per we may share.
}
altoWordsIV = \lyricmode {
  \dropLyricsVI
  See her Friend from heav’n de -- scend -- ing,
  A -- dorned with truth and grace un -- end -- ing!
  \set associatedVoice = "sopranos"
  Her light burns clear, her star doth rise.
}
altoWordsV = \lyricmode {
  \dropLyricsVI
  \set stanza = #"3. "
  Ev -- ’ry soul in Thee re -- joi -- ces;
  From men and from an -- gel -- ic voi -- ces
  \set associatedVoice = "sopranos"
  Be glo -- ry giv’n to Thee a -- lone!
  
  \set associatedVoice = "altos"
  Earth can -- not give be -- \set associatedVoice = "basses" low
  The bliss Thou dost be -- \set associatedVoice = "tenors" stow.
  Al -- le -- lu -- ia!
  Grant us to raise,
  To length of days,
  \set associatedVoice = "sopranos"
  The tri -- 
  \once \override LyricHyphen  #'minimum-length = #0.7
  \once \override LyricHyphen  #'minimum-distance = #0.7
   umph -- cho -- rus of Thy praise.
}
altoWordsVI = \lyricmode {
  \dropLyricsVI
  Now the gates of pearl re -- ceive us,
  Thy pre -- sence nev -- er more shall leave __ us,
  \set associatedVoice = "sopranos"
  We stand with An -- gels round Thy throne.
}
tenorMusic = \relative c {
  \repeat volta 2 {
    fis4 a d cis |
    d a a( gis) |
    cis2. |
    
    cis4 |
    d d a d |
    e e e4.( b8) |
    cis2. |
    
    d4 |
    d d8[ c] b4 b |
    b8[ a] g4 fis
  }
  a4 |
  d8[ cis] b[ cis] d4 a8[ g] |
  
  fis2. |
  a4 |
  a8[ b] cis4 |
  a8( b4) a8 |
  fis2 |
  
  a4 a |
  b8[ g] a2 |
  d4 |
  d cis b d |
  
  d cis d |
  d |
  a d d4. b8 |
  b[ a] g4 fis2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat volta 2 {
    d4 d d8[ fis] a[ g] |
    fis[ e] d[ cis] d[ b] e4 |
    a,2. |
    
    a'8[ g] |
    fis[ g] fis[ e] d4 d' |
    cis8[ b] a[ gis] a4( e) |
    a,2. |
    
    d8[ c] |
    b[ cis!] d4 g8[ a] b4 |
    e, a, d
  }
  cis4 |
  d e fis8[ g] a[ a,] |
  
  b2. |
  fis'4 |
  cis8[ b] a4 d8[ b] g[ a] |
  d2 |
  
  a'8[ g] fis4 |
  e4 d2 |
  d'4 |
  gis, ais b b |
  
  fis8[ g] a4 b, |
  d8[ e] |
  fis[ e] fis[ d] g,[ a] b4 |
  e a, d2 \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {

%6.14 \context {\Lyrics\override LyricText #'font-size = #0.75 }
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
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
  
}

\score {
  \unfoldRepeats

  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \midi {
    \tempo 4 = 180
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

