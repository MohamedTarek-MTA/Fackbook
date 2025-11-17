package com.fackbook.React.Enum;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum ReactType {
    LIKE("👍"),
    DISLIKE("👎"),
    LOVE("❤️"),
    ANGRY("😡"),
    SAD("😢"),
    HAHA("😂"),
    SUPPORT("💪"),
    WOW("😮");

    private final String emoji;

}
