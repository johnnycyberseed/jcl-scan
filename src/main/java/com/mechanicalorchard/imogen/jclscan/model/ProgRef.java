package com.mechanicalorchard.imogen.jclscan.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public final class ProgRef implements Program {
    private String name;
}