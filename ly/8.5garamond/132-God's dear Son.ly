\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"God’s dear Son"}}
  poet = \markup\oldStyleNum"17th Century English"
  composer = \markup\oldStyleNum"Traditional"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #132
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
  \key aes \major
  \time 2/2
  \autoBeamOff
  %\override DynamicLineSpanner.staff-padding = #0.0
  %\override DynamicLineSpanner.Y-extent = #'(0 . 0)
}

sopMusic = \relative c' {
  \repeat unfold 2 {
    \repeat volta 3 {
      \partial 4 \teeny f4 | \normalsize
      f des' c4. bes8 |
      aes4 f g e |
      f4. g8 aes4 aes |
      bes bes c4. \bar""\break \teeny f,8 | \normalsize
      
      f4 des' c4. bes8 |
      aes4 f g e |
      f4. g8 aes4 aes |
      bes bes c4. \bar""\break bes8 |
      
      c4 c ees4. c8 |
      bes4 bes ees4. des8 |
      c4 bes aes c |
      c8[ bes] aes[ g] aes4. \bar""\break aes8 |
      
      c4 c c4. c8 |
      aes4 c bes2 |
      c aes4 g |
      f c' f, e |
      f1 \break
    }
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat unfold 2{
    \teeny f4 | \normalsize
    f f e g |
    f f des c |
    c4. e8 f4 aes |
    g8[ f] e[ f] g4. \teeny f8 | \normalsize
    
    f4 f e4. g8 |
    f4 f des c |
    c4. e8 f4 aes |
    g8[ f] e[ f] e4. ees8 |
    
    ees4 aes g4. ees8 |
    ees4 aes g4. ees8 |
    ees4 e f aes |
    f ees ees4. f8 |
    
    g4 g f e |
    f ees g( f) |
    e2 f4 e |
    f f8[ ees] des4 c |
    c1
  }
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \skip1 God’s dear Son, with -- out be -- gin -- ning,
  Whom the wick -- ed Jews did scorn;
  The on -- ly wise, with -- out all sin -- ning,
  On this bless -- ed day was born;
  To save us all from sin and thrall,
  When we in Sa -- tan’s chains were bound;
  \set associatedVoice = "basses"
  And shed His blood to do us good
  With ma -- ny~a pur -- ple bleed -- ing wound.
  
  \unset associatedVoice
    \set stanza = #"4. "
  No king -- ly robes nor gold -- en trea -- sure
  Decked the birth -- day of God’s Son;
  No pomp -- ous train at all took plea -- sure
  To the King of kings to run;
  No man -- tle brave could Je -- sus have
  Up -- on His cra -- dle cold to lie;
  \set associatedVoice = "basses"
  No mu -- sic’s charms in nurse -- ’s arms
  To sing that Babe a lul -- la -- by.

}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup{\dynamic"mf  " "2. "}
  \lyricmode {
  %\markup\italic
    \skip1 Beth -- le -- hem, King Da -- vid’s ci -- ty,
    Birth -- place of that Babe we find,
  }
  \set stanza = \markup\dynamic"p   "
  \lyricmode {
    \skip1 God and Man en -- dued with pi -- ty,
    And the Sav -- ior of man -- kind:
  }
  \set stanza = \markup\dynamic"ff   "
  \lyricmode {
    Yet Jew -- ry land, with cru -- el hand,
    Both first and last His pow’r de -- nied;
  }
  \set stanza = \markup\dynamic"mf  "
  \lyricmode {
    \set associatedVoice = "basses"
    When He was born they did Him scorn,
    And showed Him mal -- ice when He died.
  }
    
  \unset associatedVoice
  \set stanza = \markup{\dynamic"mf  " "5. "}
  \lyricmode{
    "" Yet, as Ma -- ry sat in sol -- ace
    By our Sav -- ior’s cra -- dle side,
  }
  \set stanza = \markup\dynamic"p   "
  \lyricmode {
    \skip1 Hosts of An -- gels from God’s Pal -- ace,
    Sing -- ing sweet through Heav’n so wide:
  }
  \set stanza = \markup\dynamic"ff   "
  \lyricmode {
    Yea, Heav’n and earth, at Je -- su’s birth,
    With sweet mel -- o -- dious tunes a -- bound;
  }
  \set stanza = \markup\dynamic"mf  "
  \lyricmode {
    \set associatedVoice = "basses"
    And ev -- ’ry thing to Jew -- ry’s King,
    Through all the world gives cheer -- ful sound.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  No prince -- ly pal -- ace for our Sav -- ior
  In Ju -- de -- a could be found,
  \skip1 But sweet Ma -- ry’s meek be -- hav -- ior
  Pa -- tient -- ly up -- on the ground
  Her Babe did place, in vile dis -- grace,
  Where ox -- en in their stalls did feed;
  \set associatedVoice = "basses"
  No mid -- wife mild had this sweet Child,
  Nor wo -- man’s help at moth -- er’s need.
  
  \unset associatedVoice
  \set stanza = #"6. "
  \skip1 Now to Him that hath re -- deemed us
  By His death on ho -- ly Rood,
  \skip1 And as sin -- ners so e -- steemed us,
  As to buy us with His Blood,
  Yield last -- ing fame, that still the Name
  Of Je -- sus may be hon -- ored here;
  \set associatedVoice = "basses"
  And let us say that Christ -- mas Day
  Is still the best day in the year.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \repeat unfold 2 {
    \teeny aes4 | \normalsize
    aes bes g c |
    c c bes g |
    c4. bes8 aes4 c |
    bes8[ aes] g[ f] e4. \teeny aes8 | \normalsize
    
    aes4 bes g c |
    c c bes g |
    c4. bes8 aes4 c |
    bes8[ aes] g[ f] g4. g8 |
    
    aes4 aes bes4. aes8 |
    bes4 d ees4. bes8 |
    aes4 bes c ees |
    ees8[ des] c[ bes] c4. c8 |
    
    e4 e f c |
    c aes g2 |
    g aes4 bes |
    c c bes8[ aes] g4 |
    aes1
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat unfold 2 {
    \teeny f4 | \normalsize
    f bes, c e |
    f aes, bes c |
    aes'4. g8 f4 ees? |
    des des c4. \teeny f8 | \normalsize
    
    f4 bes, c e |
    f aes, bes c |
    aes'4. g8 f4 ees |
    des des c4. ees!8 |
    
    aes4 f ees4. aes8 |
    g4 f ees4. g8 |
    aes4 g f c |
    des ees aes4. f8 |
    
    c'4 bes aes g |
    f c ees( des) |
    c2 f4 g |
    aes aes, bes c |
    f1
  }
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWords
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
}

