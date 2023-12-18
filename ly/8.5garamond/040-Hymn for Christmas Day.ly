\version "2.24.0"
\include "util.ly"
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Hymn for Christmas Day"}}
  poet = \markup\oldStyleNum"Edward Caswall (1814–1878)"
  composer = \markup\oldStyleNum"Sir John Goss (1800–1880)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #040
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
global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(0 . 0)
}

sopMusic = \relative c' {
  s1*8
  
  d'4. d8 c4 b |
  a g fis2 |
  d'4. d8 c4 b |
  a g fis2 |
  
  g4. a8 g4 fis |
  e4. d8 d2 |
  d'4. b8 g4 c |
  b a g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \oneVoice
  g'4. a8 g4 fis |
  e4. d8 d2 |
  g4 a c b |
  b4. a8 a2 |
  
  g4. a8 g4 fis |
  e4. d8 d2 |
  g4 a b c |
  a4. g8 g2 \bar "||" \break

  \voiceTwo
  
  g4. g8 g4 g |
  d d d2 |
  d4. g8 g4 g |
  e d d2 |
  
  b4. b8 cis4 d |
  d4 cis d2 |
  d4. d8 e4 g |
  fis fis g2 \bar "|."
}
altoWords = {
  \dropLyricsV
  \lyricmode {
    \set stanza = "1. "
    See a -- mid the win -- ter’s snow,
    Born for us on earth be -- low,
    See the ten -- der Lamb ap -- pears,
    Prom -- ised from e -- ter -- nal years.
  }
  \break
  \set stanza = \markup\dynamic"ff "
  \lyricmode{
    \dropLyricsVII
    Hail! Thou ev -- er bless -- ed morn!
    Hail, Re -- demp -- tion’s hap -- py dawn!
    \dropLyricsIV
    \set associatedVoice = "basses"
    Sing through all Je -- ru --
    \unset associatedVoice sa -- lem,
    Christ is born in Beth -- le -- hem.
  }
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = "2. "
  Lo, with -- in a man -- ger lies
  He who built the star -- ry skies;
  He, who throned in height sub -- lime,
  Sits a -- mid the Cher -- u -- bim!
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = "3. "
  Say, ye ho -- ly Shep -- herds, say,
  What your joy -- ful news to -- day;
  Where -- fore have ye left your sheep
  On the lone -- ly moun -- tain steep?
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = "4. "
  “As we watched at dead of night,
  Lo, we saw a won -- drous light;
  An -- gels sing -- ing peace on earth,
  Told us of the Sav -- ior’s  Birth.”
}
altoWordsV = \lyricmode {
  \dropLyricsV
  \set stanza = "5. "
  Sa -- cred In -- fant, all Di -- vine,
  What a ten -- der love was Thine;
  Thus to come from high -- est bliss
  Down to such a world as this!
}
altoWordsVI = \lyricmode {
  \dropLyricsV
  \set stanza = "6. "
  Teach, O teach us, Ho -- ly Child,
  By Thy Face so meek and mild,
  Teach us to re -- sem -- ble Thee,
  In Thy sweet hu -- mil -- i -- ty!
}
altoWordsVII = \lyricmode {
  \dropLyricsV
  \set stanza = "7. "
  Vir -- gin Mo -- ther, Ma -- ry blest
  By the joys that fill thy breast,
  Pray for us, that we may prove
  Wor -- thy of the Sav -- ior’s love.
}
tenorMusic = \relative c' {
  s1*8
  
  b4. b8 e4 d |
  c b a2 |
  g4. g8 e'4 d |
  c b a2 |
  
  g4. g8 e4 d |
  b' a8[ g] fis2 |
  g4. g8 g4 e' |
  d c b2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  s1*8 
  
  g'4. g8 g4 g |
  fis g d2 |
  b4. b8 c4 g |
  a b8[ c] d2 |
  
  e4. e8 a,4 b |
  g a d( c) |
  b4. b8 c4 a |
  d d g,2 \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  << \new Voice {
        \voiceOne
        \global
        g'4.( a8 g4 fis |
        e2 d) |
        g4( a c b |
        <b g>2 <a fis>) |
        
        g4.( a8 g4 fis |
        e2 d) |
        g4( a b c a2 g) \bar "||"
     }
     \new Voice {
        \voiceTwo
        \global
        b,2 c4 d |
        g, a b c |
        d2 <g~ e>4 <g d> |
        d4 cis d2~ |
        
        d4 b c d |
        g, a b a |
        b c d e |
        c2 b \bar "||"
     }
  >>
}
pianoLH = \relative c' {
  << \new Voice {
        \voiceOne
        \global
        g2~^\p g4 d |
        e fis g a |
        b s2. |
        d,2~ d4 c |
        
        g'2~ g4 d |
        e fis g a |
        g2~ g~ |
        g4 fis g2 \bar "||"
     }
     \new Voice {
        \voiceTwo
        \global
        g,2 a4 b |
        c2 g |
        g'4 fis e g |
        s1 |
        
        b,4 g a b |
        c2 g'4 fis |
        e2 d4 c |
        d2 g, \bar "||"
     }
  >>
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
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
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

