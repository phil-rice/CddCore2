package org.cddcore.utilities;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is added to a field in a structure where it is desired to be able to view the value of that variable in the Html reports for that situation
 * It signals that this field is part of the summary. By default only the first displayable field is the summary
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.FIELD})
public @interface Summary {
}
