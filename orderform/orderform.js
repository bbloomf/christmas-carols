$(function() {
  if(!('StripeCheckout' in window)) {
    $('.only-if-checkout-available').remove();
    return;
  }
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
      $quantity = $('#quantity'),
      $subtotal = $('.txt-subtotal'),
      $total = $('.txt-total'),
      quantity = $quantity.val(),
      shipping = 400,
      MIN_QUANTITY = 5,
      subtotal,
      total,
      api_key = api_keys[window.location.hostname.match(/^localhost$/)?'test':'live'];
  var formatUSD = function(cents) {
    cents = cents.toString();
    if(cents.length <= 2) {
      return '$0.' + ("0"+cents).slice(-2);
    }
    return '$' + cents.slice(0,-2) + '.' + cents.slice(-2);
  }
  var onQuantityChange = function() {
    quantity = parseInt($quantity.val()) || 0;
    if(!quantity || quantity < MIN_QUANTITY) {
      subtotal = total = 0;
      $subtotal.text('N/A');
      $total.text('N/A');
      $('#btnPay').attr('disabled',true);
      return
    }
    $('#btnPay').attr('disabled',null);
    subtotal = quantity * 500;
    $subtotal.text(formatUSD(subtotal));
    total = subtotal + shipping;
    $total.text(formatUSD(total));
  };
  $quantity.change(onQuantityChange).keydown(function() { setTimeout(onQuantityChange); }).change();
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
        "amount": total,
        "currency": 'usd',
        "statement_descriptor": "Christmas Carol Book",
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
      description: quantity + ' copies, ' + formatUSD(total),
      amount: total,
      billingAddress: true,
      shippingAddress: true,
      zipCode: true
    });
    e.preventDefault();
  });
});
