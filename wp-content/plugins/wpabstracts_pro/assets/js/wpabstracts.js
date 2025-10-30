/**
 * Kevon Adonis
 * Copyright 2018
 * http://www.wpabstracts.com
 */

String.prototype.stripSlashes = function () {
	return this.replace(/\\(.)/gm, '$1');
};

jQuery(function () {
	alertify.defaults.overflow = false;
	alertify.defaults.notifier.delay = 0;
	alertify.defaults.glossary.title = wpabstracts.confirm_title;
	alertify.defaults.glossary.ok = wpabstracts.button_ok;
	alertify.defaults.glossary.cancel = wpabstracts.button_cancel;
	alertify.defaults.transition = 'fade';

	alertify.customSuccess = function (msg) {
		alertify.dismissAll();
		return alertify.success(msg);
	};

	alertify.customError = function (msg) {
		alertify.dismissAll();
		return alertify.error(msg);
	};
});

function wp_field_validate(itemId, error) {
	var currentError = false;
	if (itemId !== undefined) {
		itemId = itemId.replace('[]', '\\[\\]');
		var inputItem = jQuery('#' + itemId);
		if (inputItem.val() === '') {
			console.log('Empty @ ', inputItem.attr('id'));
			inputItem.addClass('required-field');
			inputItem.parent().addClass('has-error');
			currentError = true;
		} else {
			inputItem.parent().removeClass('has-error');
			inputItem.removeClass('required-field');
		}
	}
	return currentError + error;
}

function wp_radio_validate(itemName, error) {
	var currentError = false;
	if (itemName !== undefined) {
		var inputItem = jQuery('input[name="' + itemName + '"]');
		var isValid = jQuery('input[name="' + itemName + '"]:checked').length;
		if (!isValid) {
			console.log('Empty @ ', itemName);
			inputItem.parent().parent().addClass('has-error');
			currentError = true;
		} else {
			inputItem.parent().parent().removeClass('has-error');
		}
	}
	return currentError + error;
}

function wp_email_validate(email) {
	var regex = /^[\w.%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$/i;
	return regex.test(email);
}

// checks to see if a particular editor (editor_id) or an activeEditor is present or active
function wpabstracts_is_editor_active(editor_id) {
	if (typeof tinyMCE == 'undefined') {
		return false;
	}

	if (typeof editor_id == 'undefined') {
		editor = tinyMCE.activeEditor;
	} else {
		editor = tinyMCE.EditorManager.get(editor_id);
	}

	if (editor == null) {
		return false;
	}

	return !editor.isHidden();
}

function wpabstracts_add_attachment(parentId, maxAllowed) {
	jQuery('#' + parentId).append(
		'<div><input type="file" name="attachments[]"/></div>'
	);
	var count = jQuery('input[name="attachments[]"]').length;
	if (count >= maxAllowed) {
		jQuery('.add_attachments').remove();
	}
}

function wpabstracts_remove_attachment(id) {
	var attachment_id = id;
	jQuery('#attachment_' + attachment_id).remove();
	jQuery('#manage_attachments').append(
		'<input type="hidden" name="abs_remove_attachments[]" value ="' +
			attachment_id +
			'">'
	);

	//refresh upload box
	var max_uploads = jQuery('#wpabstracts_upload_limit').val();
	var current_attactments = jQuery('.wpabstracts_attachments_counter').length;
	var atts_input = jQuery('input[name=attachments\\[\\]]').length;
	var atts_remaining = max_uploads - (current_attactments + atts_input);

	for (var i = 0; i < atts_remaining; i++) {
		var new_input = jQuery(
			'<dd><input type="file" id="abs_attachmment_' +
				i +
				'" name="attachments[]"><span class="wpabstracts glyphicon glyphicon-refresh remove_file" onclick="wpabstracts_refresh_file(this);"></span></dd>'
		);
		jQuery('.wpabstracts_upload_disallowed').addClass('hidden');
		jQuery('.wpabstracts_attachments').append(new_input);
	}
	if (!current_attactments) {
		jQuery('.manage_attachments').append(
			'<p>' + wpabstracts.no_attachments + '</p>'
		);
	}
}

function wpabstracts_remove_review_attachment(att_id) {
	jQuery('#review_attachment_' + att_id).remove();
	jQuery('#review_attactments').append(
		'<input type="hidden" name="remove_review_attachment" value ="' +
			att_id +
			'">'
	);
	jQuery('#review_attactments').append(
		'<input type="file" name="review_attachment">'
	);
}

function wpabstracts_getFileExt(filename) {
	var arr = filename.split('.');
	if (arr.length === 1 || (arr[0] === '' && arr.length === 2)) {
		return '';
	}
	return arr.pop().toLowerCase();
}

function wpabstracts_validateAttachments() {
	var error = false;
	var attachments = jQuery('input[name=attachments\\[\\]]');
	for (var ii = 0; ii < attachments.length; ii++) {
		var file = attachments[ii].files[0];
		if (file) {
			var file_ext = wpabstracts_getFileExt(file.name);
			var file_size = file.size;

			// checks file type is approved
			if (wpabstracts.approved_exts.indexOf(file_ext) == -1) {
				alertify.customError(wpabstracts.file_ext_err);
				error = true;
			}
			// check is file is within size limits
			if (file_size > wpabstracts.max_atts_size) {
				alertify.customError(wpabstracts.file_size_err);
				error = true;
			}
		}
	}
	return error;
}

function wpabstracts_validateAbstract() {
	var errors = false;
	var maxCount = jQuery('.max-word-count').text();
	var content = null;
	var count = 0;
	var allEmailsValid = true;

	if (jQuery('#abstext').length) {
		// see if description is enabled
		if (wpabstracts_is_editor_active()) {
			content = jQuery.trim(tinyMCE.activeEditor.getContent());
		} else {
			content = jQuery.trim(jQuery('#abstext').val());
		}
		if (content.length > 0) {
			count = content.replace(/\s+/gi, ' ').split(' ').length;

			if (count > maxCount) {
				alertify.customError(wpabstracts.word_count_err);
				return;
			}
		} else {
			errors = false;
			alertify.customError(wpabstracts.desc_required);
			return;
		}
	}

	jQuery('#abs_form .wpa_event_input')
		.not('.optional')
		.each(function () {
			errors = wp_field_validate(jQuery(this).attr('id'), errors);
		});

	jQuery("#abs_form :input[type='email']").each(function () {
		if (!wp_email_validate(jQuery(this).val())) {
			jQuery(this).addClass('required-field');
			allEmailsValid = false;
		} else {
			jQuery(this).removeClass('required-field');
		}
	});

	var abs_terms = jQuery("#abs_form input[name='abs_terms']");

	if (jQuery(abs_terms).length > 0 && !jQuery(abs_terms).prop('checked')) {
		jQuery(abs_terms).parent().addClass('wpabstracts text-danger');
		errors = true;
	} else {
		jQuery(abs_terms).parent().removeClass('text-danger');
	}

	var att_error = wpabstracts_validateAttachments();

	if (errors) {
		alertify.customError(wpabstracts.required_fields);
	}

	if (!allEmailsValid) {
		alertify.customError(wpabstracts.invalid_email);
	}

	if (!errors && !att_error && allEmailsValid) {
		alertify.dismissAll();
		alertify.success('Saving...');
		jQuery('#abs_form').submit();
	}
}

function wpabstracts_validateUser() {
	var errors = false;
	jQuery('#wpa_profile_form input, #wpa_profile_form select')
		.not('.optional')
		.each(function () {
			if (jQuery(this).parent().hasClass('required')) {
				errors = wp_field_validate(jQuery(this).attr('id'), errors);
			}
		});

	var terms = jQuery('#terms_conditions');
	if (terms.prop('checked') === false) {
		terms.parent().addClass('has-error');
		errors = true;
	} else {
		terms.parent().removeClass('has-error');
	}

	if (errors) {
		alertify.customError(wpabstracts.required_fields);
	} else {
		jQuery('#wpa_profile_form').submit();
	}
}

function wpabstracts_validateLogin() {
	var errors = false;
	jQuery(
		"#wpa_login_form input[type='text'], #wpa_login_form  input[type='password']"
	).each(function () {
		if (jQuery(this).parent().hasClass('required')) {
			errors = wp_field_validate(jQuery(this).attr('id'), errors);
		}
	});
	if (errors) {
		alertify.customError(wpabstracts.required_fields);
	} else {
		jQuery('#wpa_login_form').submit();
	}
}

function wpabstracts_validateReview() {
	var errors = false;
	jQuery('#wpabs_review_form input[type=text], #wpabs_review_form select')
		.not('.optional')
		.each(function () {
			errors = wp_field_validate(jQuery(this).attr('id'), errors);
		});

	jQuery('#wpabs_review_form input[type=radio]')
		.not('.optional')
		.each(function () {
			errors = wp_radio_validate(jQuery(this).attr('name'), errors);
		});

	var content = null;
	if (wpabstracts_is_editor_active()) {
		content = jQuery.trim(tinyMCE.activeEditor.getContent());
	} else {
		content = jQuery.trim(jQuery('#abs_comments').val());
	}
	if (jQuery('#abs_comments').length && content.length < 1) {
		errors = true;
		jQuery('#abs_review_comments_error').addClass('bg-danger');
	} else {
		jQuery('#abs_review_comments_error').removeClass('bg-danger');
	}
	if (errors) {
		alertify.customError(wpabstracts.required_fields);
	} else {
		jQuery('#wpabs_review_form').submit();
	}
}

function wpabstracts_add_coauthor() {
	var author_box = jQuery('.author_box:first').clone();
	jQuery(author_box).find('input').val('');
	jQuery('#coauthors_table').append(author_box);
}

function wpabstracts_delete_coauthor() {
	if (jQuery('#coauthors_table .author_box').length > 1) {
		jQuery('#coauthors_table').find('.author_box:last').remove();
	}
}

function wpabstracts_add_presenter() {
	var presenter_box = jQuery('.presenter_box:first').clone();
	jQuery(presenter_box).find('input').val('');
	jQuery('#presenter_table').append(presenter_box);
}

function wpabstracts_delete_presenter() {
	if (jQuery('#presenter_table .presenter_box').length > 1) {
		jQuery('#presenter_table').find('.presenter_box:last').remove();
	}
}

function wpabstracts_add_status() {
	var timeStamp = -new Date().valueOf();
	var status_box = jQuery('.wpa_status_default:first').clone();
	jQuery(status_box)
		.removeClass('hidden')
		.removeClass('wpa_status_default')
		.addClass('wpa_status')
		.attr('id', timeStamp);
	jQuery(status_box)
		.find('input')
		.attr('name', 'wpabstracts_status[' + timeStamp + ']');
	jQuery('#wpa_status_container').append(status_box);
}

function wpabstracts_delete_status(elem, id) {
	if (jQuery('.wpa_status').length > 1) {
		alertify.confirm(wpabstracts.confirm_status_delete, function (e) {
			if (e) {
				jQuery(elem).parent('.wpa_status').remove();
				jQuery('#wpa_status_container').append(
					'<input type="hidden" name="wpabstracts_delete_status[]" value ="' +
						id +
						'">'
				);
			}
		});
	} else {
		alertify.customError(wpabstracts.status_required);
	}
}

function wpabstracts_add_topic() {
	var html =
		'<p><input class="wpabstracts form-control" type="text" name="topics[]" id="topics[]" /></p>';
	jQuery('#topics_table').append(html);
}

function wpabstracts_delete_topic(id) {
	alertify.confirm(wpabstracts.confirm_topic_delete, function (e) {
		if (e) {
			location.href =
				'?page=wpabstracts&tab=events&subtab=topics&task=delete&id=' + id;
		}
	});
}

function wpabstracts_delete_abstracts(ids, page, isBulk) {
	var warning = isBulk
		? wpabstracts.confirm_abstracts_delete
		: wpabstracts.confirm_abstract_delete;
	var formAction = '';
	var abs_ids = jQuery(ids).toArray();
	alertify.confirm(warning, function (e) {
		if (e) {
			if (location.pathname.indexOf('admin') >= 0) {
				formAction = '?page=wpabstracts&tab=abstracts&task=delete';
				var delete_form = jQuery('<form method="post" id="delete_form">').attr(
					'action',
					formAction
				);
				jQuery(delete_form).append(
					'<input name="nonce" type="hidden" value="' + wpabstracts.nonce + '">'
				);
				jQuery(delete_form).append(
					'<input name="isBulk" type="hidden" value="' + isBulk + '">'
				);
				jQuery(delete_form).append(
					'<input name="page" type="hidden" value="' + page + '">'
				);
				abs_ids.forEach(function (id) {
					jQuery(delete_form).append(
						'<input name="abs_ids[]" type="hidden" value="' + id + '">'
					);
				});
				jQuery('body').append(delete_form);
				jQuery('#delete_form').submit();
			} else {
				location.href =
					'?task=delete_abstract&id=' +
					abs_ids[0] +
					`&_wpnonce=${wpabstracts.nonce}`;
			}
		}
	});
}

function wpabstracts_delete_review(id, page) {
	alertify.confirm(wpabstracts.confirm_review_delete, function (e) {
		if (e) {
			if (location.pathname.indexOf('admin') >= 0) {
				location.href =
					'?page=wpabstracts&tab=reviews&task=delete&id=' +
					id +
					'&paged=' +
					page;
			} else {
				location.href = '?task=delete_review&id=' + id + '';
			}
		}
	});
}

function wpabstracts_delete_attachment(id, page) {
	alertify.confirm(wpabstracts.confirm_atts_delete, function (e) {
		if (e) {
			location.href =
				'?page=wpabstracts&tab=attachments&task=delete&id=' +
				id +
				'&paged=' +
				page;
		}
	});
}

function wpabstracts_delete_user(id, page) {
	alertify.confirm(wpabstracts.confirm_user_delete, function (e) {
		if (e) {
			location.href =
				'?page=wpabstracts&tab=users&task=delete&id=' + id + '&paged=' + page;
		}
	});
}

function wpabstracts_delete_event(id, nonce) {
	alertify.prompt(wpabstracts.confirm_event_delete).setting({
		onok: function (closeEvent) {
			var entry = jQuery('.ajs-input').val();
			if (entry && 'DELETE' == entry.toUpperCase()) {
				location.href =
					'?page=wpabstracts&tab=events&task=delete&id=' +
					id +
					'&_wpnonce=' +
					nonce;
			} else {
				return false;
			}
		},
		labels: {
			ok: wpabstracts.button_delete,
			cancel: wpabstracts.button_cancel,
		},
	});
}

function wpabstracts_delete_template(id) {
	alertify.confirm(wpabstracts.confirm_template_delete, function (e) {
		if (e) {
			location.href =
				'?page=wpabstracts&tab=emails&subtab=templates&task=delete&id=' + id;
		}
	});
}

function wpabstracts_restore_user_formbuilder() {
	alertify.confirm(wpabstracts.confirm_form_restore, function (e) {
		if (e) {
			location.href =
				'?page=wpabstracts&tab=users&subtab=formbuilder&task=restore';
		}
	});
}

function wpabstracts_load_reviewers(ids, paged, isBulk) {
	jQuery.ajax({
		url: wpabstracts.ajaxurl,
		type: 'POST',
		data: {
			action: 'loadreviewers',
			abs_ids: jQuery(ids).toArray(),
			isBulk: isBulk,
			page: paged,
			nonce: wpabstracts.nonce,
		},
		success: function (response) {
			jQuery('body').append('<div id="assign_reviewer"></div>');
			jQuery('#assign_reviewer').html('').html(response.data);
			jQuery('#assign_reviewer')
				.dialog({
					width: 550,
					modal: true,
					closeOnEscape: true,
					dialogClass: 'assign-reviewer',
					closeText: '',
					title: '',
					buttons: [
						{
							text: 'Cancel',
							click: function () {
								jQuery('#assign_reviewer').dialog('close');
							},
							class: 'wpa_button cancel',
						},
						{
							text: 'Save',
							click: function () {
								jQuery('#assign_form').submit();
							},
							class: 'wpa_button success',
						},
					],
				})
				.dialog('open');
		},
		error: function (error) {
			alertify.customError(wpabstracts.error_ajax);
		},
	});
}

function wpabstracts_load_status(ids, paged, isBulk) {
	jQuery.ajax({
		url: wpabstracts.ajaxurl,
		type: 'POST',
		data: {
			action: 'loadstatus',
			nonce: wpabstracts.nonce,
			abs_ids: jQuery(ids).toArray(),
			isBulk: isBulk,
			page: paged,
		},
		success: function (response) {
			jQuery('body').append('<div id="change_status"></div>');
			jQuery('#change_status').html('').html(response.data);
			jQuery('#change_status')
				.dialog({
					width: 400,
					modal: true,
					closeOnEscape: true,
					dialogClass: 'assign-reviewer',
					closeText: '',
					title: wpabstracts.change_status,
					buttons: [
						{
							text: 'Cancel',
							click: function () {
								jQuery('#change_status').dialog('close');
							},
							class: 'wpa_button cancel',
						},
						{
							text: 'Change Status',
							click: function () {
								jQuery('#change_status_form').submit();
							},
							class: 'wpa_button success',
						},
					],
				})
				.dialog('open');
		},
		error: function () {
			alertify.customError(wpabstracts.error_ajax);
		},
	});
}

function wpabstracts_load_topics(eventId) {
	jQuery.ajax({
		url: wpabstracts.ajaxurl,
		type: 'POST',
		data: {
			action: 'loadtopics',
			event_id: eventId,
			nonce: wpabstracts.nonce,
		},
		success: function (response) {
			if (response.success) {
				const topics = response.data;
				console.log(topics);
				const $dropdown = jQuery('#topic_id');
				$dropdown.empty();
				topics.forEach((topic) => {
					$dropdown.append(
						jQuery('<option>', {
							value: topic.id,
							text: topic.name,
						})
					);
				});
			} else {
				alertify.customError(wpabstracts.error_ajax);
			}
		},
		error: function () {
			alertify.customError(wpabstracts.error_ajax);
		},
	});
}

function wpabstracts_updateWordCount() {
	var content = null;
	var notified = false;
	var count = 0;
	var counterCont = jQuery('.abs-word-count');
	var remainingCont = jQuery('.abs-word-remaining');
	var maxCount = jQuery('.max-word-count').text();

	if (wpabstracts_is_editor_active()) {
		content = jQuery.trim(tinyMCE.activeEditor.getContent());
	} else {
		content = jQuery.trim(jQuery('#abstext').val());
	}
	if (jQuery('#abstext').length > 0 && content.length > 0) {
		count = content.replace(/\s+/gi, ' ').split(' ').length;
	}
	counterCont.text(count);
	remainingCont.text(maxCount - count);
	if (count > maxCount && !notified) {
		remainingCont.css('color', 'red');
		counterCont.css('color', 'red');
	} else {
		counterCont.css('color', 'green');
		remainingCont.css('color', 'green');
	}
	if (count > maxCount) {
		alertify.customError(wpabstracts.word_count_err);
		return;
	}
}

function wpabstracts_validateTemplate() {
	var errors = false;
	jQuery('form#emailtemplate .wpa_event_input').each(function () {
		errors = wp_field_validate(jQuery(this).attr('id'), errors);
	});
	if (!jQuery('#email_body').val()) {
		jQuery('#email_body').addClass('has-error');
		errors = true;
	} else {
		jQuery('#email_body').removeClass('has-error');
	}
	if (errors) {
		alertify.customError(wpabstracts.required_fields);
	} else {
		jQuery('#emailtemplate').submit();
	}
}

function wpabstracts_validateEvent() {
	var errors = false;
	jQuery('form#abs_event_form .wpa_event_input').each(function () {
		errors = wp_field_validate(jQuery(this).attr('id'), errors);
	});
	if (errors) {
		alertify.customError(wpabstracts.required_fields);
	} else {
		jQuery('#abs_event_form').submit();
	}
}

function wpabstracts_add_reviewer() {
	var reviewer_selection = jQuery('.empty_reviewer').clone();
	jQuery(reviewer_selection).removeClass('empty_reviewer');
	jQuery(reviewer_selection).removeClass('hidden');
	jQuery('#reviewers_table').append(reviewer_selection);

	// reset header columns and empty messaging
	if (!jQuery('.reviewer_selection').not('.empty_reviewer').length) {
		jQuery('.no_reviewers').removeClass('hidden');
		jQuery('.row_titles').addClass('hidden');
	} else {
		jQuery('.no_reviewers').addClass('hidden');
		jQuery('.row_titles').removeClass('hidden');
	}
}

function wpabstracts_delete_reviewer(elem) {
	jQuery(elem).closest('.reviewer_selection').remove();
	var rid = jQuery(elem)
		.closest('.reviewer_selection')
		.find('option:selected')
		.val();
	jQuery('#reviewers_table').append(
		'<input type="hidden" name="remove_reviewers[]" value ="' + rid + '">'
	);
	jQuery('input[name="notify_reviewers[]"][value="' + rid + '"]').remove();

	// reset header columns and empty messaging
	if (!jQuery('.reviewer_selection').not('.empty_reviewer').length) {
		jQuery('.no_reviewers').removeClass('hidden');
		jQuery('.row_titles').addClass('hidden');
	} else {
		jQuery('.no_reviewers').addClass('hidden');
		jQuery('.row_titles').removeClass('hidden');
	}
}

function wpabstracts_toggle_reviewer_notify(elem) {
	var rid = jQuery(elem)
		.closest('.reviewer_selection')
		.find('option:selected')
		.val();
	var checked = jQuery(elem).prop('checked');
	if (checked) {
		jQuery('#reviewers_table').append(
			'<input type="hidden" name="notify_reviewers[]" value ="' + rid + '">'
		);
	} else {
		jQuery('input[name="notify_reviewers[]"][value="' + rid + '"]').remove();
	}
	console.log({ rid, checked });
}

function wpabstracts_copy_to_clipboard(textToCopy) {
	if (!navigator.clipboard) {
		var textArea = document.createElement('textarea');
		textArea.value = textToCopy;
		document.body.appendChild(textArea);
		textArea.focus();
		textArea.select();
		try {
			var success = document.execCommand('copy');
			if (success) {
				alertify.success(wpabstracts.copy_success);
			} else {
				alertify.customError(wpabstracts.copy_failure);
			}
		} catch (err) {
			alertify.customError(wpabstracts.copy_failure);
		}
		document.body.removeChild(textArea);
	} else {
		navigator.clipboard.writeText(textToCopy).then(
			function () {
				alertify.success(wpabstracts.copy_success);
			},
			function (err) {
				alertify.customError(wpabstracts.copy_failure);
			}
		);
	}
}

function wpabstracts_refresh_file(elem) {
	var $el = jQuery(elem).parent().find('input');
	$el.wrap('<form>').closest('form').get(0).reset();
	$el.unwrap();
}

function wpabstracts_restore_titles(nonce) {
	alertify.confirm(wpabstracts.confirm_title_restore, function (e) {
		if (e) {
			location.href =
				'?page=wpabstracts&tab=settings&subtab=titles&task=reset&_wpnonce=' +
				nonce;
		}
	});
}

function wpabstracts_sync_user_meta(nonce) {
	alertify.prompt(wpabstracts.confirm_usermeta_sync).setting({
		onok: function (closeEvent) {
			var entry = jQuery('.ajs-input').val();
			if (entry && 'SYNC' == entry.toUpperCase()) {
				location.href =
					'?page=wpabstracts&tab=users&subtab=settings&task=sync&_wpnonce=' +
					nonce;
			} else {
				return false;
			}
		},
		labels: {
			ok: 'SYNC',
			cancel: 'Cancel',
		},
	});
}

function wpabstracts_load_maillog(log_id) {
	var data = {
		action: 'wpamaillog',
		log_id,
	};
	jQuery.post(wpabstracts.ajaxurl, data).done(function (_log) {
		var log = JSON.parse(_log);
		jQuery('body').append('<div id="wpa_mail_log"></div>');
		jQuery('#wpa_mail_log').html('').html(log.body);
		jQuery('#wpa_mail_log')
			.dialog({
				width: 650,
				maxHeight: 750,
				modal: true,
				closeOnEscape: true,
				closeText: '',
				title: 'Email Log',
				buttons: [
					{
						text: 'Close',
						click: function () {
							jQuery('#wpa_mail_log').dialog('close');
						},
						class: 'wpa_button cancel',
					},
				],
			})
			.dialog('open');
	});
}

function wpabstracts_validateGenericForm(form_id) {
	var errors = false;
	jQuery('#' + form_id + ' input[type=text]')
		.not('.optional')
		.each(function () {
			errors = wp_field_validate(jQuery(this).attr('id'), errors);
		});
	jQuery('#' + form_id + ' select')
		.not('.optional')
		.each(function () {
			errors = wp_field_validate(jQuery(this).attr('id'), errors);
		});

	if (errors) {
		alertify.error(wpabstracts.required_fields);
	}

	if (!errors) {
		jQuery('#' + form_id).submit();
	}
}

function wpabstracts_redirect_after(url, time) {
	const _do_countdown = (intervalId) => {
		jQuery('#wpabstracts_countdown').text(time);
		if (time === 0) {
			clearInterval(intervalId);
			window.location.href = url;
		} else {
			time--;
		}
	};
	var intervalId = setInterval(function () {
		_do_countdown(intervalId);
	}, 1000);
}

function wpabstracts_save_regform(formdata) {
	jQuery.ajax({
		url: wpabstracts.ajaxurl,
		type: 'POST',
		data: {
			action: 'wpabs_save_regform',
			form_data: formdata,
			nonce: wpabstracts.nonce,
		},
		success: function (response) {
			if (response.success) {
				alertify.success(response.data.message);
			} else {
				alertify.customError(response.data.message);
			}
		},
		error: function () {
			alertify.customError(response.data.message);
		},
	});
}
