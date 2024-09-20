import pygame
import random

# Inicialización de Pygame
pygame.init()

# Configuración de pantalla
SCREEN_WIDTH = 800
SCREEN_HEIGHT = 600
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Juego de Tanques")

# Colores
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
GREEN = (0, 255, 0)
RED = (255, 0, 0)
BLUE = (0, 0, 255)

# Tamaño de los bloques (tanques, muros)
BLOCK_SIZE = 40

clock = pygame.time.Clock()

# --- CLASES ---

class Tank:
    def __init__(self, x, y, color):
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)
        self.color = color
        self.speed = 5  # Velocidad de movimiento

    def draw(self, screen):
        pygame.draw.rect(screen, self.color, self.rect)

    def move(self, x_change, y_change, walls):
        # Nueva posición
        new_rect = self.rect.move(x_change, y_change)

        # Verificar colisión con muros
        for wall in walls:
            if new_rect.colliderect(wall.rect):
                return  # No moverse si choca con un muro

        self.rect = new_rect


class Wall:
    def __init__(self, x, y):
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)

    def draw(self, screen):
        pygame.draw.rect(screen, BLUE, self.rect)


class EnemyTank(Tank):
    def update(self, player_pos, walls):
        # Movimiento básico hacia el jugador
        if self.rect.x < player_pos[0]:
            x_change = self.speed
        elif self.rect.x > player_pos[0]:
            x_change = -self.speed
        else:
            x_change = 0

        if self.rect.y < player_pos[1]:
            y_change = self.speed
        elif self.rect.y > player_pos[1]:
            y_change = -self.speed
        else:
            y_change = 0

        self.move(x_change, y_change, walls)


class Bullet:
    def __init__(self, x, y, direction):
        self.rect = pygame.Rect(x, y, 5, 5)
        self.speed = 10
        self.direction = direction  # ('UP', 'DOWN', 'LEFT', 'RIGHT')

    def move(self):
        if self.direction == 'UP':
            self.rect.y -= self.speed
        elif self.direction == 'DOWN':
            self.rect.y += self.speed
        elif self.direction == 'LEFT':
            self.rect.x -= self.speed
        elif self.direction == 'RIGHT':
            self.rect.x += self.speed

    def draw(self, screen):
        pygame.draw.rect(screen, WHITE, self.rect)

    def off_screen(self):
        return (self.rect.x < 0 or self.rect.x > SCREEN_WIDTH or 
                self.rect.y < 0 or self.rect.y > SCREEN_HEIGHT)


# --- FUNCIONES AUXILIARES ---

def handle_bullets(bullets, enemies):
    """ Mueve las balas y verifica colisiones con los enemigos """
    for bullet in bullets[:]:  # Usamos [:] para copiar la lista mientras la iteramos
        bullet.move()

        # Verificar si una bala golpea a un enemigo
        for enemy in enemies[:]:  # También hacemos copia de enemigos para eliminar mientras iteramos
            if bullet.rect.colliderect(enemy.rect):
                enemies.remove(enemy)  # Eliminar enemigo
                bullets.remove(bullet)  # Eliminar bala
                break

        # Eliminar la bala si sale de la pantalla
        if bullet.off_screen():
            bullets.remove(bullet)


# --- CONFIGURACIÓN INICIAL ---

player = Tank(100, 100, GREEN)  # Tanque del jugador
walls = [Wall(200, 200), Wall(240, 200)]  # Lista de muros
enemies = [EnemyTank(400, 400, RED), EnemyTank(600, 400, RED)]  # Tanques enemigos
bullets = []  # Lista de balas

# --- BUCLE PRINCIPAL ---

running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

        # Disparar bala al presionar la barra espaciadora
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                # Crear una bala en la dirección actual del jugador
                if keys[pygame.K_UP]:
                    bullet = Bullet(player.rect.centerx, player.rect.top, 'UP')
                elif keys[pygame.K_DOWN]:
                    bullet = Bullet(player.rect.centerx, player.rect.bottom, 'DOWN')
                elif keys[pygame.K_LEFT]:
                    bullet = Bullet(player.rect.left, player.rect.centery, 'LEFT')
                elif keys[pygame.K_RIGHT]:
                    bullet = Bullet(player.rect.right, player.rect.centery, 'RIGHT')
                bullets.append(bullet)

    # Manejo del movimiento del tanque del jugador
    keys = pygame.key.get_pressed()
    x_change, y_change = 0, 0
    if keys[pygame.K_LEFT]:
        x_change = -player.speed
    if keys[pygame.K_RIGHT]:
        x_change = player.speed
    if keys[pygame.K_UP]:
        y_change = -player.speed
    if keys[pygame.K_DOWN]:
        y_change = player.speed

    player.move(x_change, y_change, walls)

    # Actualizar y mover los enemigos hacia el jugador
    for enemy in enemies:
        enemy.update(player.rect.topleft, walls)

    # Manejo de balas
    handle_bullets(bullets, enemies)

    # Dibujar los elementos en la pantalla
    screen.fill(BLACK)
    player.draw(screen)
    
    for wall in walls:
        wall.draw(screen)

    for enemy in enemies:
        enemy.draw(screen)

    for bullet in bullets:
        bullet.draw(screen)

    pygame.display.flip()
    clock.tick(30)  # 30 FPS

pygame.quit()
