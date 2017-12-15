$(function() {
  var api_keys = {
    test: {
      public: 'pk_test_sqnok8syjX5SqZ8e3s9mJ0Iy',
      charge: 'rk_test_4x7GP4HST3jgiSTsL69i67Q5'
    }, 
    live: {
      public: 'pk_live_uU9tPAfqwliuPVeKQsRhQMKG',
      charge: 'rk_live_SjB98QHbCEyBygDLrkKIKcOP'
    }
  },
      $stripeScript = $('#stripeScript'),
      $quantity = $('#quantity'),
      $subtotal = $('.txt-subtotal'),
      $total = $('.txt-total'),
      quantity = $quantity.val(),
      shipping = 4,
      subtotal,
      total,
      api_key = api_keys[window.location.hostname.match(/^localhost$/)?'test':'live'];
  var onQuantityChange = function() {
    quantity = parseInt($quantity.val()) || 0;
    subtotal = quantity * 5;
    $subtotal.text('$' + subtotal + '.00');
    total = subtotal + shipping;
    $total.text('$' + total + '.00');
  };
  $quantity.change(onQuantityChange).keyup(onQuantityChange).change();
  var handler = StripeCheckout.configure({
    key: api_key.public,
    allowRememberMe: false,
    image: 'http://acollectionofchristmascarols.com/images/cover.jpg',
    locale: 'auto',
    token: function(token, args) {
      $('#btnPay').attr('disabled',true).text('Processing...Please wait');
      $quantity.attr('disabled',true);
      $('#btnCancelOrder').hide();
      var data = {
        "amount": total * 100,
        "currency": 'usd',
        "description": quantity + ' copies of A Collection of Christmas Carols',
        "source": token.id
      };
      var keys = Object.keys(args);
      for(var i in keys) {
        var key = keys[i];
        if(key.match(/^shipping_/)) {
          var newKey = 'metadata[' + key + ']';
          data[newKey] = args[key];
        }
      }
      $.post({
        url: 'https://api.stripe.com/v1/charges',
        headers: {
          authorization: "Bearer " + api_key.charge
        },
        data: data,
        success: function(data, textStatus, jqXHR) {
          $('#btnPay').remove();
          $('#btnCancelOrder').text('OK').show();
          $('#confirmation-code').text(data.id);
          $('.thank-you').show();    
        },
        error: function(jqXHR, textStatus, errorThrown) {
          $('#btnPay').text('Pay');
          $('#btnCancelOrder').show();
          $quantity.attr('disabled',null);
          $('#btnPay').attr('disabled',null);
          if(jqXHR.responseJSON && jqXHR.responseJSON.error && jqXHR.responseJSON.error.message) {
            $('.txt-error').text('Error: ' + jqXHR.responseJSON.error.message);
          } else {
            $('.txt-error').text('There was an error processing your card.  Please try again.');
          }
          $('.error').show();
        }
      });
    }
  });

  $('#btnPay').click(function(e) {
    $('.error').hide();
    handler.open({
      name: 'A Collection of Christmas Carols',
      description: quantity + ' copies, $' + total + '.00',
      amount: total * 100,
      billingAddress: true,
      shippingAddress: true,
      zipCode: true
    });
    e.preventDefault();
  });
});
